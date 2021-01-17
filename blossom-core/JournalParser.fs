module JournalParser

open System

open FParsec

open Shared
open Types
open ParserShared

type UserState =
  {
    IndentSize : int
    IndentCount : int
  }
  with
    static member Default = {
      IndentCount = 0
      IndentSize = 2
    }

(*
  Journal parsing returns a data in a raw journal (RJournal) format which is the bottom
  of a processing pipeline. This is not widely checked / validated / expanded but is tagged
  with meta items, such as position on major elements, to help users with locating errors
  in their journal files.

  It is close to a tokenized format. Some types are shared with the Journal class which
  is the general format used for analysis, as there is no point duplicating types unnecessarily.

  The number of types here is attempted to be minimised, and the parser kept as simple as possible.
  Note in particular that account hierarchies are not exploded into lists here, and utilise Account itself
  instead of the list version.
*)

type RAmount =
  | Un of decimal
  | Ve of Value
  | Tf of Value * Value
  | Th of Value * decimal
  | Cr of Value * Value
  | Cl of Value * Value

type RContraAccount =
   | NoCAccount
   | Self
   | CAccount of Account

type RPostingElement =
  | Posting of account:Account * amount:RAmount option * contra:RContraAccount
  | PComment of Comment
  | PCommented of RPostingElement * Comment

type RJournalElement =
  // Operational
  | Indent of int
  // Standard
  | Header of JournalMeta
  | Import of string
  | Comment of Comment
  // Declarations
  | Account of AccountDecl
  | Commodity of CommodityDecl
  // Core entries
  | Entry of flagged: bool * date:DateTime * payee:string option * narrative:string * tags:string Set * xs:RPostingElement list
  | Prices of commodity:Commodity * measure:Commodity * xs:(DateTime * decimal) list
  | Split of date:DateTime * commodity:Commodity * pre:int * post:int
  | Assertion of date:DateTime * account:Account * value:Value
  // A element that has a comment associated with it
  | Commented of RJournalElement * Comment

type private RSubElement =
  | SComment of Comment
  | SCommodity of Commodity
  | SCG of Account
  | SNote of string
  | SName of string
  | SCommodityClass of CommodityClass
  | SValuationMode of ValuationMode
  | SMeasure of Commodity
  | SUnderlying of Commodity
  | SMultiplier of int
  | SExternalIdent of string * string
  | SMTM
  | SPropagate

// TODO Add line number to parser for the root items

// Basic parser helpers
// To get comment association correct, we are precise on whitespace and layout (to borrow)
//  the Haskell terminology
let nSpaces0 = skipMany (skipChar ' ')
let nSpaces1 = skipMany1 (skipChar ' ')

let str = pstring
let sstr = skipString
let sstr1 s = skipString s >>. nSpaces1

let indented p =
  let sp n m = skipArray (n*m) (skipChar ' ') >>? p
  getUserState >>= fun s -> sp s.IndentCount s.IndentSize

let increaseIndent p =
  getUserState >>= fun st -> let st2 = {st with IndentCount = st.IndentCount+1}
                             setUserState st2 >>. p .>> setUserState st

// Sub parsers
let pdate = tuple3 (pint32 .>> pchar '-') (pint32 .>> pchar '-') pint32 |>> DateTime
let pnumber = pfloat|>> decimal

let pCommodity =
  let first = letter <|> digit <|> anyOf "."
  many1Chars2 first (letter <|> digit <|> anyOf ".:-()_") |>> Types.Commodity

let pValue =
  pnumber .>> nSpaces1 .>>. pCommodity |>> Value

let pRAmount =
  let pcr = pValue .>> nSpaces1 .>> sstr1 "->" .>>. pValue |>> Cr
  let pcl = pValue .>> nSpaces1 .>> sstr1 "<-" .>>. pValue |>> Cl
  let ptf = pValue .>> nSpaces1 .>> skipChar '@' .>> nSpaces1 .>>. pValue |>> Tf
  let pth = pValue .>> nSpaces1 .>> skipChar '@' .>> nSpaces1 .>>. pnumber |>> Th
  let pv = pValue |>> Ve
  let pu = pnumber |>> Un
  choice [attempt pcr; attempt pcl; attempt ptf; attempt pth; attempt pv; pu]

let pCommodityClass : Parser<CommodityClass, UserState> =
  choice [stringReturn "Currency" Currency
          stringReturn "Equity"   Equity
          stringReturn "Option"   Option
          stringReturn "Future"   Future]

let pValuationMode : Parser<ValuationMode, UserState> =
  choice [stringReturn "Latest" Latest
          stringReturn "Historical" Historical]

// Account name elements and parsing
let accountValidChars = letter <|> digit <|> anyOf "()[]{}" <|> (pchar ' ' .>> notFollowedBy (pchar ' ') |> attempt)
let pAccountElt = many1Chars2 letter accountValidChars
let pAccountHierarchy =
  many1Till (pAccountElt .>> (opt (pchar ':'))) (followedBy (sstr "  " <|> skipNewline)) |>> fun ts -> String.Join(":", ts) |> Types.Account

// RSubElement Parsers
let private spCommodity = sstr1 "commodity" >>. pCommodity |>> SCommodity
let private spCG = sstr1 "cg" >>. pAccountHierarchy |>> SCG
let private spNote = sstr1 "note" >>. restOfLine false |>> SNote
let private spName = sstr1 "name" >>. restOfLine false |>> SName
let private spCommodityClass = sstr1 "class" >>. pCommodityClass |>> SCommodityClass
let private spValuationMode = sstr1 "valuation" >>. pValuationMode |>> SValuationMode
let private spMeasure = sstr1 "measure" >>. pCommodity |>> SMeasure
let private spUnderlying = sstr1 "underlying" >>. pCommodity |>> SUnderlying
let private spMultiplier = sstr1 "multiplier" >>. pint32 |>> SMultiplier
let private spExternalIdent = sstr1 "externalid" >>. many1CharsTill (letter <|> digit <|> anyOf ".") (pchar ' ')
                                                 .>> nSpaces0
                                                 .>>. restOfLine false
                                                 |>> SExternalIdent
let private spMTM = sstr "mtm" >>% SMTM
let private spPropagate = sstr "propagate" >>% SPropagate

let glse xs pred = xs |> List.choose pred
                      |> List.tryLast

// RJournalElement Parsers
// TODO Support detecting comments on restOfLine items (or parse till ';' etc)
let wrapCommented c elt = match c with | Some c -> Commented(elt, c)
                                       | None   -> elt

let pComment0 = pchar ';' >>. restOfLine false |>> Types.Comment
let pComment = pComment0 |>> Comment

let pOptLineComment p = p .>> nSpaces0 .>>. opt pComment0 .>> optional newline

let pIndent =
  let pval = sstr1 ".indent" >>. pint32 .>> nSpaces0 .>> skipNewline
  pval >>= fun i -> (updateUserState (fun u -> {u with IndentSize = i}) >>. preturn (Indent i))

let pHeader =
  let subitems = (choice [spCommodity; spCG; spNote] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "journal" >>. restOfLine true .>>. increaseIndent subitems
    |>> fun (t, ss) ->
          Header {Name = t.Trim()
                  Commodity = glse ss (function (SCommodity x) -> Some x | _ -> None)
                  CapitalGains = glse ss (function (SCG x) -> Some x | _ -> None)
                  Note = glse ss (function (SNote x) -> Some x | _ -> None)}

let pImport =
  let filename = restOfLine true
  sstr1 "import" >>. filename |>> Import

let pAccountDecl =
  let subitems = (choice [spCommodity; spCG; spNote; spValuationMode; spPropagate] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "account" >>. pOptLineComment pAccountHierarchy .>>. increaseIndent subitems
    |>> fun ((a, c), ss) -> Account {Account = a
                                     Commodity = glse ss (function (SCommodity x) -> Some x | _ -> None)
                                     Note = glse ss (function (SNote x) -> Some x | _ -> None)
                                     CapitalGains = glse ss (function (SCG x) -> Some x | _ -> None)
                                     ValuationMode = glse ss (function (SValuationMode x) -> Some x | _ -> None)
                                                        |> Option.defaultValue Historical
                                     Propagate = List.contains SPropagate ss}
                            |> wrapCommented c

let pCommodityDecl =
  let subitems = (choice [spName; spMeasure; spUnderlying; spCommodityClass; spMultiplier; spMTM; spExternalIdent] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "commodity" >>. pOptLineComment pCommodity .>>. increaseIndent subitems
    |>> fun ((t, c), ss) ->
          RJournalElement.Commodity {Symbol = t
                                     Measure = glse ss (function (SMeasure m) -> Some m | _ -> None)
                                     Underlying = glse ss (function (SUnderlying m) -> Some m | _ -> None)
                                     Name = glse ss (function (SName n) -> Some n | _ -> None)
                                     Klass = glse ss (function (SCommodityClass c) -> Some c | _ -> None)
                                     Multiplier = glse ss (function (SMultiplier m) -> Some (decimal m) | _ -> None)
                                     ExternalIdents = ss |> List.choose (function (SExternalIdent (a,b)) -> Some (a,b) | _ -> None) |> Map.ofList
                                     Mtm = List.contains SMTM ss}
          |> wrapCommented c

let pPostingEntry =
  let contraAccount = choice [attempt (skipChar '~' >>. nSpaces0 >>. pAccountHierarchy |>> CAccount)
                              skipChar '~' >>. preturn Self] |> opt
                        |>> function Some c -> c | None -> NoCAccount
  let pp = pOptLineComment (tuple3 (pAccountHierarchy .>> nSpaces0) (opt (attempt (pRAmount .>> nSpaces0))) (nSpaces0 >>. contraAccount))
                |>> fun ((h,a,ca), cm) -> let ps = Posting (h, a, ca)
                                          match cm with | Some c -> PCommented (ps, c) | _ -> ps
  let pc = pComment0 .>> skipNewline |>> PComment
  choice [attempt pc; pp]

let pEntry =
  // TODO: could write it as a parser later..
  let spn (n:string) = n.Split([|'|'|], 2) |> List.ofArray
                                           |> function | [x] -> (None, x) | x::xs -> (Some (x.Trim()), List.head xs) | _ -> (None, n)
  let phashline = indented (sepBy (pchar '#' >>. many1Chars (letter <|> digit)) nSpaces1 .>> newline)
  let subitems = indented pPostingEntry
  let pflag = opt (pchar '*' .>> nSpaces1) |>> Option.isSome

  pdate .>> nSpaces1 .>>. pflag .>>. (pOptLineComment (manyChars (noneOf ";\n")))
        .>>. increaseIndent (opt phashline .>>. many1 subitems)
        |>> fun (((d, f), (n, cm)), (hs, xs)) ->
              let p, n = spn n
              let tags = defaultArg hs [] |> set
              Entry (flagged = f, date = d, payee = p, narrative = n.Trim(), tags=tags, xs=xs)
                 |> wrapCommented cm

let pPrices =
  let subitems = (pdate .>> nSpaces1 .>>. pnumber .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "prices" >>. pOptLineComment (pCommodity .>> nSpaces1 .>>. pCommodity) .>>. increaseIndent subitems
    |>> fun (((c,m), cm), xs) -> Prices (commodity = c, measure = m, xs = xs)
                                   |> wrapCommented cm

let pSplit =
  sstr1 "split" >>. pOptLineComment (tuple4 (pdate .>> nSpaces1) (pCommodity .>> nSpaces1) (pint32 .>> nSpaces1) pint32)
    |>> fun ((d, c, ou, nu), cm) -> Split (date = d, commodity = c, pre = ou, post = nu) |> wrapCommented cm

let pAssertion =
  sstr1 "assert" >>. pOptLineComment (tuple3 (pdate .>> nSpaces1) (pAccountHierarchy .>> nSpaces1) pValue)
    |>> fun ((d, t, n), c) -> Assertion (date = d, account = t, value = n) |> wrapCommented c

let pRJournal =
  let parsers = [ pIndent;
                  pHeader; pImport; pComment;
                  pAccountDecl; pCommodityDecl;
                  pEntry; pPrices; pSplit;
                  pAssertion]
  many (getPosition .>>. choice parsers .>> skipMany newline) .>> eof

let loadRJournal filename =
  let result = runParser pRJournal UserState.Default (FromFile filename)
  result