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
  | U of decimal
  | V of Value
  | Tf of Value * Value
  | Th of Value * decimal
  | Cr of Value * Value
  | Cl of Value * Value

type RContraAccount =
   | NoCAccount
   | Self
   | CAccount of Account

type RJournalElement =
  // Operational
  | Indent of int
  // Standard
  | Header of JournalMeta
  | Import of string
  | Comment of string
  // Declarations
  | Account of AccountDecl
  | Commodity of CommodityDecl
  // Core entries
  | Entry of date:DateTime * payee:string option * narrative:string * xs:(Account * RAmount option * RContraAccount) list
  | Prices of commodity:Commodity * measure:Commodity * xs:(DateTime * decimal) list
  | Split of date:DateTime * commodity:Commodity * pre:int * post:int
  | Assertion of date:DateTime * account:Account * amount:RAmount
  // A element that has a comment associated with it is a rec element
  | Commented of RJournalElement * string

type RSubElement =
  | Commodity of Commodity
  | CG of Account
  | Note of string
  | Name of string
  | CommodityClass of CommodityClass
  | Measure of Commodity
  | Multiplier of int
  | MTM

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
  let sp n m = skipArray (n*m) (skipChar ' ') >>. p
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
  let pcr= pValue .>> nSpaces1 .>> sstr1 "->" .>>. pValue |>> Cr
  let pcl = pValue .>> nSpaces1 .>> sstr1 "<-" .>>. pValue |>> Cl
  let ptf = pValue .>> nSpaces1 .>> skipChar '@' .>> nSpaces1 .>>. pValue |>> Tf
  let pth = pValue .>> nSpaces1 .>> skipChar '@' .>> nSpaces1 .>>. pnumber |>> Th
  let pv = pValue |>> V
  let pu = pnumber |>> U
  choice [attempt pcr; attempt pcl; attempt ptf; attempt pth; attempt pv; pu]

let pCommodityClass : Parser<CommodityClass, UserState> =
  choice [stringReturn "Currency" Currency
          stringReturn "Equity"   Equity
          stringReturn "Option"   Option
          stringReturn "Future"   Future]

// Account name elements and parsing
let accountValidChars = letter <|> digit <|> anyOf "()[]{}" <|> (pchar ' ' .>> notFollowedBy (pchar ' ') |> attempt)
let pAccountElt = many1Chars2 letter accountValidChars
let pAccountHierarchy =
  many1Till (pAccountElt .>> (opt (pchar ':'))) (followedBy (sstr "  " <|> skipNewline)) |>> fun ts -> String.Join(":", ts) |> Types.Account

// RSubElement Parsers
let spCommodity = sstr1 "commodity" >>. pCommodity |>> Commodity
let spCG = sstr1 "cg" >>. pAccountHierarchy |>> CG
let spNote = sstr1 "note" >>. restOfLine false |>> Note
let spName = sstr1 "name" >>. restOfLine false |>> Name
let spCommodityClass = sstr1 "class" >>. pCommodityClass |>> CommodityClass
let spMeasure = sstr1 "measure" >>. pCommodity |>> Measure
let spMultiplier = sstr1 "multiplier" >>. pint32 |>> Multiplier
let spMTM : Parser<RSubElement, UserState> = sstr "mtm" >>% MTM

let glse xs pred = xs |> List.choose pred
                      |> List.tryLast

// RJournalElement Parsers
// TODO Support detecting comments on restOfLine items (or parse till ';' etc)
let wrapCommented c elt = match c with | Some c -> Commented(elt, c)
                                       | None   -> elt

let pComment0 = pchar ';' >>. restOfLine false
let pComment = pComment0 |>> Comment

let pOptLineComment p = p .>>. opt pComment0 .>> optional newline

let pIndent =
  let pval = sstr1 ".indent" >>. pint32 .>> nSpaces0 .>> skipNewline
  pval >>= fun i -> (updateUserState (fun u -> {u with IndentSize = i}) >>. preturn (Indent i))

let pHeader =
  let subitems = (choice [spCommodity; spCG; spNote] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "journal" >>. pOptLineComment (manyChars (noneOf ";\n")) .>>. increaseIndent subitems
    |>> fun ((t, c), ss) ->
          Header {Name = t
                  Commodity = glse ss (function (Commodity x) -> Some x | _ -> None)
                  CapitalGains = glse ss (function (CG x) -> Some x | _ -> None)
                  Note = glse ss (function (Note x) -> Some x | _ -> None)}
          |> wrapCommented c

let pImport =
  let filename = restOfLine true
  sstr1 "import" >>. filename |>> Import

let pAccountDecl =
  let subitems = (choice [spCommodity; spCG; spNote] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "account" >>. pOptLineComment (pAccountHierarchy .>> nSpaces0) .>>. increaseIndent subitems
    |>> fun ((a, c), ss) -> Account {Account = a
                                     Commodity = glse ss (function (Commodity x) -> Some x | _ -> None)
                                     Note = glse ss (function (Note x) -> Some x | _ -> None)
                                     CapitalGains = glse ss (function (CG x) -> Some x | _ -> None)}
                            |> wrapCommented c

let pCommodityDecl =
  let subitems = (choice [spName; spMeasure; spCommodityClass; spMultiplier; spMTM] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "commodity" >>. pOptLineComment (pCommodity .>> nSpaces0) .>>. increaseIndent subitems
    |>> fun ((t, c), ss) ->
          RJournalElement.Commodity {Symbol = t
                                     Measure = glse ss (function (Measure m) -> Some m | _ -> None)
                                     Name = glse ss (function (Name n) -> Some n | _ -> None)
                                     Klass = glse ss (function (CommodityClass c) -> Some c | _ -> None)
                                     Multiplier = glse ss (function (Multiplier m) -> Some (decimal m) | _ -> None)
                                     Mtm = List.contains MTM ss}
          |> wrapCommented c

let pEntry =
  let subsubitems = pAccountHierarchy .>> nSpaces0 .>> skipNewline
  let contraAccount = choice [attempt (skipChar '~' >>. nSpaces0 >>. pAccountHierarchy |>> CAccount)
                              skipChar '~' >>. preturn Self]
                        |> opt
                        |>> function Some c -> c | None -> NoCAccount
  let subitems = (pAccountHierarchy .>> nSpaces0) .>>. (opt (attempt (pRAmount .>> nSpaces0))) .>> nSpaces0 .>>. contraAccount .>> skipNewline |> indented |>> fun ((a,b), c) -> (a,b,c)
  pdate .>> nSpaces1 .>>. restOfLine true .>>. increaseIndent (many1 subitems)
    |>> fun ((d, n), xs) -> Entry (date = d, payee = None, narrative = n, xs=xs)

let pPrices =
  let subitems = (pdate .>> nSpaces1 .>>. pnumber .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "prices" >>. pOptLineComment (pCommodity .>> nSpaces1 .>>. pCommodity .>> nSpaces0) .>>. increaseIndent subitems
    |>> fun (((c,m), cm), xs) -> Prices (commodity = c, measure = m, xs = xs)
                                   |> wrapCommented cm

let pSplit =
  sstr1 "split" >>. pOptLineComment (tuple4 (pdate .>> nSpaces1) (pCommodity .>> nSpaces1) (pint32 .>> nSpaces1) (pint32 .>> nSpaces0))
    |>> fun ((d, c, ou, nu), cm) -> Split (date = d, commodity = c, pre = ou, post = nu) |> wrapCommented cm

let pAssertion =
  sstr1 "assert" >>. pOptLineComment (tuple3 (pdate .>> nSpaces1) (pAccountHierarchy .>> nSpaces1) (pRAmount .>> nSpaces0))
    |>> fun ((d, t, n), c) -> Assertion (date = d, account = t, amount = n) |> wrapCommented c

let pRJournal =
  let parsers = [ pIndent;
                  pHeader; pImport; pComment;
                  pAccountDecl; pCommodityDecl;
                  pEntry; pPrices; pSplit;
                  pAssertion]
  many (choice parsers .>> skipMany newline) .>> eof

let loadRJournal filename =
  let result = runParser pRJournal UserState.Default (FromFile filename)
  result