module JournalParser

open System

open FParsec

open Shared
open Types
open Definitions
open ParserShared

type UserState =
  {
    IndentSize : int
    IndentCount : int
    AccountConvention: AccountConvention option
  }
  with
    static member Default = {
      IndentCount = 0
      IndentSize = 2
      AccountConvention = None
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
  | Un of decimal                         // Unlabelled plain amount    "109.4"
  | Ve of Value                           // Normal measured value      "109.4 USD"
  // trading full (with measure) or half (unmeasured price), with optional lot names
  | Tf of Value * Value   * LotName list  // 100 TSLA @ 899 USD [lot5]
  | Th of Value * decimal * LotName list  // -100 TSLA @ 899 [lot5, lot3]
  | Cr of Value * Value                   // Right conversion           "100 USD -> 96 EUR"
  | Cl of Value * Value                   // Left conversion            "100 USD <- 96 EUR"

type RPostingElement =
  | Posting of account:Account * amount:RAmount option * contra:Account option
  | PComment of Comment
  | PCommented of RPostingElement * Comment

type RElement =
  | Comment2 of text:string
  | BasicEntry of flagged: bool * payee:string option * narrative:string * tags:string Set * xs:RPostingElement list
  | Assertion of account:Account * value:Value
  | Price of commodity:Commodity * value:Value
  | Split of commodity:Commodity * pre:int * post:int

type RJournalElement =
  // Structural
  | Indent of int
  | StartRegion of name:string
  | EndRegion
  | Comment1 of text:string
  // Operational
  | Header of JournalMeta
  | Import of string
  | Account of AccountDecl
  | Commodity of CommodityDecl
  // Core entries
  | Item of timestamp:DateTime * element:RElement
  | Prices of commodity:Commodity * measure:Commodity * xs:(DateTime * decimal) list

type private RSubElement =
  | SComment of Comment
  | SCommodity of Commodity
  | SCG of Account
  | SNote of string
  | SAccountConvention of AccountConvention
  | SName of string
  | SCommodityClass of CommodityClass
  | SValuationMode of ValuationMode
  | SMeasure of Commodity
  | SQuoteDP of int
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

let pLotName =
  let sq = letter <|> digit <|> anyOf "._-"
  many1Chars2 letter sq |>> CustomLotName

let pRAmount =
  let pcr = pValue .>> nSpaces1 .>> sstr1 "->" .>>. pValue |>> Cr
  let pcl = pValue .>> nSpaces1 .>> sstr1 "<-" .>>. pValue |>> Cl
  let pLotNames = between (pchar '[') (pchar ']') (sepBy pLotName (pchar ',' .>>. nSpaces0))
  let ptf = pValue .>> nSpaces1 .>> skipChar '@' .>> nSpaces1 .>>. pValue
              .>>. opt (nSpaces1 >>. pLotNames)
              |>> fun ((q, c), ns) -> Tf (q, c, Option.defaultValue [uid() |> AutoLotName] ns)
  let pth = pValue .>> nSpaces1 .>> skipChar '@' .>> nSpaces1 .>>. pnumber
              .>>. opt (nSpaces1 >>. pLotNames)
              |>> fun ((q, c), ns) -> Th (q, c, Option.defaultValue [uid() |> AutoLotName] ns)
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
let pAccountConvention : Parser<AccountConvention, UserState> =
  choice [stringReturn "F5" Financial5
          stringReturn "F7" Financial7]

let accountValidChars = letter <|> digit <|> anyOf "()[]{}" <|> (pchar ' ' .>> notFollowedBy (pchar ' ') |> attempt)
let pAccountElt = many1Chars2 letter accountValidChars
let pAcccountElts = many1Till (pAccountElt .>> (opt (pchar ':'))) (followedBy (sstr "  " <|> skipNewline <|> skipChar '/'))
let pAccountHierarchy =
  let pVAccount = opt (pchar '/' >>. manyChars accountValidChars)
  let plain =
    // Parsing depends upon the convention. If no convention, anything goes.
    // A convention mandates that the stub is part of a hierarchy (no 1 level accounts)
    getUserState >>=
      fun st -> match st.AccountConvention with
                  | None -> pAcccountElts
                  | Some ac -> let pStub = ac |> getAccountConventionStubs |> List.map (fun x -> pstring x .>> pchar ':') |> choice
                               pipe2 pStub pAcccountElts (fun a b -> [a] @ b)
  plain .>>. pVAccount |>> fun (ts, va) -> let hs = String.concat ":" ts
                                           match va with | Some v -> VirtualisedAccount (hs, v)
                                                         | None   -> Types.Account hs

// RSubElement Parsers
let private spCommodity = sstr1 "commodity" >>. pCommodity |>> SCommodity
let private spCG = sstr1 "cg" >>. pAccountHierarchy |>> SCG
let private spNote = sstr1 "note" >>. restOfLine false |>> SNote
let private spConvention = sstr1 "convention" >>. pAccountConvention |>> SAccountConvention
let private spName = sstr1 "name" >>. restOfLine false |>> SName
let private spCommodityClass = sstr1 "class" >>. pCommodityClass |>> SCommodityClass
let private spValuationMode = sstr1 "valuation" >>. pValuationMode |>> SValuationMode
let private spMeasure = sstr1 "measure" >>. pCommodity |>> SMeasure
let private spQuoteDP = sstr1 "dp" >>. pint32 |>> SQuoteDP
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

// TODO Support detecting comments on restOfLine items (or parse till ';' etc)
// let wrapCommented c elt = match c with | Some c -> Commented(elt, c)
//                                        | None   -> elt

// let pComment0 xs = anyOf xs >>. restOfLine false |>> Types.Comment
// let pComment = pComment0 ";*" |>> Comment

// let pOptLineComment p = p .>> nSpaces0 .>>. opt (pComment0 ";").>> optional newline

// RJournalElement Parsers

let pIndent =
  let pval = sstr1 ".indent" >>. pint32 .>> nSpaces0 .>> skipNewline
  pval >>= fun i -> (updateUserState (fun u -> {u with IndentSize = i}) >>. preturn (Indent i))

let pStartRegion = nSpaces0 >>. sstr1 "#region" >>. restOfLine true |>> StartRegion

let pEndRegion = nSpaces0 >>. stringReturn "#endregion" EndRegion

let pHeader =
  let getConvention x = glse x (function (SAccountConvention x) -> Some x | _ -> None)
  let subitems = (choice [spCommodity; spCG; spNote; spConvention] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "journal" >>. restOfLine true .>>. increaseIndent subitems
    >>= fun (t, ss) -> (updateUserState (fun u -> {u with AccountConvention = getConvention ss}) >>. preturn (t, ss))
    |>> fun (t, ss) ->
          Header {Name = t.Trim()
                  Commodity = glse ss (function (SCommodity x) -> Some x | _ -> None)
                  CapitalGains = glse ss (function (SCG x) -> Some x | _ -> None)
                  Note = glse ss (function (SNote x) -> Some x | _ -> None)
                  Convention = getConvention ss}

let pComment1 =
  nSpaces0 >>. skipAnyOf ";*" >>. restOfLine true |>> Comment1

let pImport =
  let filename = restOfLine true
  sstr1 "import" >>. filename |>> Import

let pAccountDecl =
  let subitems = (choice [spCommodity; spCG; spNote; spValuationMode; spPropagate] .>> nSpaces0 .>> skipNewline) |> indented |> many
  let accountHierarchy = pAcccountElts |>> (String.concat ":" >> Types.Account)
  sstr1 "account" >>. accountHierarchy .>> skipNewline .>>. increaseIndent subitems
    |>> fun (a, ss) -> Account {Account = a
                                Commodity = glse ss (function (SCommodity x) -> Some x | _ -> None)
                                Note = glse ss (function (SNote x) -> Some x | _ -> None)
                                CapitalGains = glse ss (function (SCG x) -> Some x | _ -> None)
                                ValuationMode = glse ss (function (SValuationMode x) -> Some x | _ -> None)
                                                   |> Option.defaultValue Historical
                                Propagate = List.contains SPropagate ss}

let pCommodityDecl =
  let subitems = (choice [spName; spMeasure; spQuoteDP; spUnderlying; spCommodityClass; spMultiplier; spMTM; spExternalIdent] .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "commodity" >>. pCommodity .>> skipNewline .>>. increaseIndent subitems
    |>> fun (t, ss) ->
          RJournalElement.Commodity {Symbol = t
                                     Measure = glse ss (function (SMeasure m) -> Some m | _ -> None)
                                     QuoteDP = glse ss (function (SQuoteDP i) -> Some i | _ -> None)
                                     Underlying = glse ss (function (SUnderlying m) -> Some m | _ -> None)
                                     Name = glse ss (function (SName n) -> Some n | _ -> None)
                                     Klass = glse ss (function (SCommodityClass c) -> Some c | _ -> None)
                                     Multiplier = glse ss (function (SMultiplier m) -> Some (decimal m) | _ -> None)
                                     ExternalIdents = ss |> List.choose (function (SExternalIdent (a,b)) -> Some (a,b) | _ -> None) |> Map.ofList
                                     Mtm = List.contains SMTM ss}

let pElement =
  let pComment = sstr1 "comment" >>. restOfLine false |>> Comment2
  let pAssertion = sstr1 "assert" >>. pAccountHierarchy .>> nSpaces1 .>>. pValue |>> Assertion
  let pPrice = sstr1 "price" >>. pCommodity .>> nSpaces1 .>>. pValue |>> Price
  let pSplit = sstr1 "split" >>. tuple3 (pCommodity .>> nSpaces1) (pint32 .>> nSpaces1) pint32 |>> Split

  // basic entry
  let pPostingEntry =
    let contraAccount = choice [attempt (skipChar '~' >>. nSpaces0 >>. pAccountHierarchy |>> Choice1Of2)
                                skipChar '~' >>. preturn (Choice2Of2 ())] |> opt
    let pp = tuple3 (pAccountHierarchy .>> nSpaces0) (opt (attempt (pRAmount .>> nSpaces0))) (nSpaces0 >>. contraAccount)
                  |>> fun (h,a,ca) -> let ca2 = Option.map (function Choice1Of2 x -> x | Choice2Of2 _ -> h) ca
                                      Posting (h, a, ca2)
    pp .>> skipNewline

  let pBasicEntry =
    // TODO: could write it as a parser later..
    let spn (n:string) = n.Split([|'|'|], 2) |> List.ofArray
                                             |> function | [x] -> (None, x) | x::xs -> (Some (x.Trim()), List.head xs) | _ -> (None, n)
    let phashline = indented (sepBy (pchar '#' >>. many1Chars (letter <|> digit)) nSpaces1 .>> newline)
    let subitems = indented pPostingEntry
    let flag = stringReturn "*" true <|> preturn false
    flag .>>. manyChars (noneOf ";\n") .>> skipNewline .>>. increaseIndent (opt phashline .>>. many1 subitems)
      |>> fun ((f, x), (y, z)) -> let p, n = spn x
                                  let tags = Option.defaultValue [] y |> set
                                  BasicEntry (f, p, n, tags, z)

  choice [pComment; pAssertion; pPrice; pSplit; pBasicEntry]

let pItem =
  pdate .>> nSpaces1 .>>. pElement |>> Item

let pPrices =
  let subitems = (pdate .>> nSpaces1 .>>. pnumber .>> nSpaces0 .>> skipNewline) |> indented |> many
  sstr1 "prices" >>. pCommodity .>> nSpaces1 .>>. pCommodity .>> skipNewline .>>. increaseIndent subitems
    |>> fun ((c, m), xs) -> Prices (commodity = c, measure = m, xs = xs)

let pRJournal =
  let parsers = [
    pIndent; pStartRegion; pEndRegion; pComment1;
    pHeader; pImport; pAccountDecl; pCommodityDecl;
    pItem; pPrices
  ]
  spaces >>. many (getPosition .>>. choice parsers .>> skipMany newline) .>> eof

let loadRJournal filename =
  let result = runParser pRJournal UserState.Default (FromFile filename)
  result