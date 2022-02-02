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

  It is close to a tokenized format. Some types are similar to upper types which makes the
  code a little longer, but simpler to read and process.

  The number of types here is attempted to be minimised, and the parser kept as simple as possible.
  Note in particular that account hierarchies are not exploded into lists here, and utilise Account itself
  instead of the list version.
*)

type DAssertion = {
  Account: Account
  Value: Value
  Comment: string option
}

type DSplit = {
  Commodity: Commodity
  K1: decimal
  K2: decimal
}

type Contra = CS | CV of Account

type DTransferEntry =
  | Posting of account:Account * value:Value option * contra:Contra option
                  * comment:string option
                  * splits:(Account * decimal * string option) list
  | PComment of string
  | PNote of string

type DTransfer = {
  Payee: string option
  Narrative: string
  Tags: string Set
  Entries: DTransferEntry list
}

type DDividend = {
  Account: Account
  Asset: Commodity
  Quantity: decimal
  PerUnitValue: Value
  PayDate: DateTime option
  Settlement: Account option
  Receivable: Account option
  Income: Account option
}

type DTrade = {
  Account: Account
  Settlement: Account option
  CapitalGains: Account option
  Asset: Commodity
  Quantity: decimal
  Keff: decimal
  PerUnitPrice: Value
  LotName: string list
  Reference: string option
  Expenses: (Account * Value * Contra option) list
  Note: string list
}

type RElement =
  | Comment2 of text:string
  | Assertion of DAssertion
  | Split of DSplit
  | Transfer of DTransfer
  | Dividend of DDividend
  | Trade of DTrade

type RJournalElement =
  // Structural
  | Indent of int
  | StartRegion of name:string
  | EndRegion
  | Comment1 of text:string
  // Operational
  | Header of JournalMeta
  | Import of string
  // Declarations and defintions
  | Account of AccountDecl
  | Commodity of CommodityDecl
  // Core elements
  | Item of sequence:SQ * flagged:bool * element:RElement
  | Prices of commodity:Commodity * measure:Commodity * xs:(DateTime * decimal) list

type private RSubElement =
  | SAccount of string * Account
  | SAccountConvention of AccountConvention
  | SComment of Comment
  | SCommodity of string * Commodity
  | SCommodityClass of CommodityClass
  | SExpensePosting of Account * Value * Contra option
  | SExternalIdent of string * string
  | SLotNames of string list
  | SMTM
  | SMultiplier of int
  | SName of string
  | SNote of string
  | SPaydate of DateTime
  | SReference of string
  | SQuoteDP of int
  | SValuationMode of ValuationMode

// TODO Add line number to parser for the root items

// Basic parser helpers
// To get comment association correct, we are precise on whitespace and layout (to borrow)
//  the Haskell terminology
let nSpaces0 = skipMany (skipChar ' ')
let nSpaces1 = skipMany1 (skipChar ' ')
let rol b = restOfLine b |>> fun s -> s.TrimEnd()

let lcmt = nSpaces0 >>. opt (skipChar ';' >>. rol false) .>> newline

let str = pstring
let sstr = skipString
let sstr1 s = skipString s >>. nSpaces1
let p0 p = p .>> nSpaces0
let p1 p = p .>> nSpaces1

let indented p =
  let sp n m = skipString (String.replicate (n*m) " ") >>? p
  getUserState >>= fun s -> sp s.IndentCount s.IndentSize

let increaseIndent p =
  getUserState >>= fun st -> let st2 = {st with IndentCount = st.IndentCount+1}
                             setUserState st2 >>. p .>> setUserState st

// Primitive parsers
let pdate = tuple3 (pint32 .>> pchar '-') (pint32 .>> pchar '-') pint32 |>> DateTime

let psequence : Parser<SQ, UserState> = pdate .>>. opt (sstr "/" >>. puint32)

let pnumber = pfloat |>> decimal

let pCommodity =
  let first = letter <|> digit <|> anyOf "."
  many1Chars2 first (letter <|> digit <|> anyOf ".:-()_") |>> Types.Commodity

let pValue = pnumber .>> nSpaces1 .>>. pCommodity |>> Value

let pSubItems ss = (choice ss .>> nSpaces0 .>> skipNewline) |> indented |> many

let pWordPlus = many1Chars2 letter (letter <|> digit <|> anyOf "._-")

let pCommodityClass : Parser<CommodityClass, UserState> =
  choice [stringReturn "Currency" Currency
          stringReturn "Equity"   Equity
          stringReturn "ETF"      ETF
          stringReturn "Option"   Option
          stringReturn "Future"   Future
          stringReturn "Fund"     Fund
          stringReturn "Other"    Other]

let pValuationMode : Parser<ValuationMode, UserState> =
  choice [stringReturn "Latest" Latest
          stringReturn "Historical" Historical]

// Account name elements and parsing (Note, accounts can no longer have spaces inside)
let pAccountConvention : Parser<AccountConvention, UserState> =
  choice [stringReturn "F5" Financial5
          stringReturn "F7" Financial7]

let pAccount =
  let accountValidChars = letter <|> digit <|> anyOf "()[]{}_"
  let pAccountElt = many1Chars2 (upper <|> digit) accountValidChars
  let hierarchy = sepBy1 pAccountElt (pchar ':')
  // Parsing depends upon the convention. If no convention, anything goes.
  // A convention mandates that the stub is part of a hierarchy (no 1 level accounts)
  let parser =
    getUserState >>=
      fun st -> match st.AccountConvention with
                  | None    -> hierarchy
                  | Some ac -> let stub = ac |> getAccountConventionStubs |> List.map (fun x -> pstring x .>> skipChar ':') |> choice
                               pipe2 stub hierarchy (fun a b -> [a] @ b)
  let pvirt = opt (skipChar '/' >>. pAccountElt)
  pipe2 parser pvirt (fun ms v -> let main = String.concat ":" ms
                                  match v with Some vv -> Types.Account2 (main, vv) | None -> Types.Account main)

let pPosting =
  let fullContra = nSpaces1 >>? sstr "~" >>? opt pAccount
  let splitContra = sstr "~" >>. tuple3 (p1 pAccount) pnumber lcmt
  p0 pAccount .>>. opt (pValue .>>. opt fullContra) .>>. lcmt
    .>>. increaseIndent (splitContra |> indented |> many)
    |>> fun (((a, vx), cmt), splits) -> Posting <| match vx with
                                                     | None                    -> (a, None, None, cmt, splits)
                                                     | Some (v, None)          -> (a, Some v, None, cmt, splits)
                                                     | Some (v, Some None)     -> (a, Some v, Some CS, cmt, splits)
                                                     | Some (v, Some (Some x)) -> (a, Some v, Some (CV x), cmt, splits)

let pPostingM =
  let contra = sstr "~" >>. opt pAccount
  p0 pAccount .>>. pValue .>>. opt (nSpaces1 >>. contra)
    |>> fun ((a, v), c) -> match c with
                             | None          -> (a, v, None)
                             | Some None     -> (a, v, Some CS)
                             | Some (Some x) -> (a, v, Some (CV x))

// RSubElement Parsers
let private spCommodity ctag = sstr1 ctag >>. pCommodity |>> curry SCommodity ctag
let private spAccount atag = sstr1 atag >>. pAccount |>> curry SAccount atag
let private spNote = sstr1 "note" >>. rol false |>> SNote
let private spConvention = sstr1 "convention" >>. pAccountConvention |>> SAccountConvention
let private spName = sstr1 "name" >>. rol false |>> SName
let private spCommodityClass = sstr1 "class" >>. pCommodityClass |>> SCommodityClass
let private spValuationMode = sstr1 "valuation" >>. pValuationMode |>> SValuationMode
let private spQuoteDP = sstr1 "dp" >>. pint32 |>> SQuoteDP
let private spMultiplier = sstr1 "multiplier" >>. pint32 |>> SMultiplier
let private spExternalIdent = sstr1 "externalid" >>. many1CharsTill (letter <|> digit <|> anyOf ".") (pchar ' ')
                                                 .>> nSpaces0
                                                 .>>. rol false
                                                 |>> SExternalIdent
let private spPaydate = sstr1 "paydate" >>. pdate |>> SPaydate
let private spMTM = sstr "mtm" >>% SMTM
let private spLotNames = sstr1 "lot" >>. sepBy1 pWordPlus (skipChar ',' >>. nSpaces0) |>> SLotNames
let private spReference = sstr1 "reference" >>. pWordPlus |>> SReference
let private spExpensePosting = sstr1 "expense" >>. pPostingM |>> SExpensePosting

// RJournalElement Parsers

let pIndent =
  let pval = sstr1 ".indent" >>. pint32 .>> nSpaces0 .>> skipNewline
  pval >>= fun i -> (updateUserState (fun u -> {u with IndentSize = i}) >>. preturn (Indent i))

let pStartRegion = sstr1 "#region" >>. rol true |>> StartRegion
let pEndRegion = stringReturn "#endregion" EndRegion
let pComment1 = skipAnyOf ";*" >>. rol true |>> Comment1

let pHeader =
  let getConvention = List.tryPick (function (SAccountConvention x) -> Some x | _ -> None)
  let subitems = [spCommodity "commodity"; spNote; spConvention]
  sstr1 "journal" >>. rol true .>>. increaseIndent (pSubItems subitems)
    >>= fun (t, ss) -> (updateUserState (fun u -> {u with AccountConvention = getConvention ss}) >>. preturn (t, ss))
    |>> fun (t, ss) ->
          Header {Name = t
                  Commodity = ss |> List.tryPick (function SCommodity ("commodity", x) -> Some x | _ -> None)
                  Note = ss |> List.tryPick (function SNote x -> Some x | _ -> None)
                  Convention = getConvention ss}

let pImport = sstr1 "import" >>. rol true |>> Import

let pAccountDecl =
  let subitems = [spCommodity "commodity"; spNote; spValuationMode]
  sstr1 "account" >>. pAccount .>> skipNewline .>>. increaseIndent (pSubItems subitems)
    |>> fun (a, ss) -> Account {Account = a
                                Commodity = ss |> List.tryPick (function SCommodity ("commodity", x) -> Some x | _ -> None)
                                Note = ss |> List.tryPick  (function SNote x -> Some x | _ -> None)
                                ValuationMode = ss |> List.tryPick (function SValuationMode x -> Some x | _ -> None)
                                                   |> Option.defaultValue Historical}

let pCommodityDecl =
  let subitems = [spName; spQuoteDP; spCommodity "underlying"; spCommodityClass; spMultiplier; spMTM; spExternalIdent]
  sstr1 "commodity" >>. pCommodity .>> skipNewline .>>. increaseIndent (pSubItems subitems)
    |>> fun (t, ss) -> Commodity {Symbol = t
                                  Measure = internalDefaultCommodity // will be set "properly" later
                                  QuoteDP = ss |> List.tryPick (function SQuoteDP i -> Some i | _ -> None)
                                  Underlying = ss |> List.tryPick (function SCommodity ("underlying", m) -> Some m | _ -> None)
                                  Name = ss |> List.tryPick  (function SName n -> Some n | _ -> None)
                                  Klass = ss |> List.tryPick  (function SCommodityClass c -> Some c | _ -> None)
                                  Multiplier = ss |> List.tryPick (function SMultiplier m -> Some (decimal m) | _ -> None)
                                  ExternalIdents = ss |> List.choose (function SExternalIdent (a,b) -> Some (a,b) | _ -> None) |> Map.ofList
                                  Mtm = List.contains SMTM ss}

let pElement =
  // Fairly simple entries
  let pComment = sstr1 "comment" >>. rol false |>> Comment2
  let pAssertion = sstr1 "assert" >>. p1 pAccount .>>. pValue .>>. lcmt |>> fun ((a, v), cmt) -> Assertion {Account = a; Value = v; Comment = cmt}
  let pSplit = sstr1 "split" >>. tuple3 (p1 pCommodity) (p1 pnumber ) pnumber |>> fun (c, k1, k2) -> Split {Commodity = c; K1 = k1; K2 = k2}

  // Composite entries
  let pTransfer =
    let spn (n:string) = n.Split([|'|'|], 2) |> List.ofArray
                                             |> function | [x] -> (None, x) | x::xs -> (Some (x.Trim()), List.head xs) | _ -> (None, n)
    let pNote = sstr1 "note" >>. rol true |>> PNote
    let subitems = choice [pPosting; pNote; lcmt |>> (Option.get >> PComment)] |> indented |> many
    rol true .>>. increaseIndent subitems
      |>> fun (header, entries) -> let payee, narrative = spn header
                                   Transfer {Payee = payee; Narrative = narrative; Tags = Set.empty; Entries = entries}

  let pDividend =
    let subitems = [spPaydate; spAccount "settlement"; spAccount "receivable"; spAccount "income"]
    sstr1 "dividend" >>. (p1 pAccount) .>>. (p1 pValue .>> sstr1 "@" .>>. pValue) .>> skipNewline .>>. increaseIndent (pSubItems subitems)
      |>> fun ((account, ((q, c), v)), ss) -> let paydate = ss |> List.tryPick (function SPaydate d -> Some d | _ -> None)
                                              let s = ss |> List.tryPick (function SAccount ("settlement", d) -> Some d | _ -> None)
                                              let r = ss |> List.tryPick (function SAccount ("receivable", d) -> Some d | _ -> None)
                                              let i =  ss |> List.tryPick  (function SAccount ("income", d) -> Some d | _ -> None)
                                              Dividend {Account = account; Asset = c; Quantity = q; 
                                                        PerUnitValue = v; PayDate = paydate;
                                                        Settlement = s; Receivable = r; Income = i}

  let pTrade =
    let subitems = [spLotNames; spReference; spAccount "settlement"; spAccount "cg"; spExpensePosting; spNote]
    sstr1 "trade" >>. (p1 pAccount) .>>. (p1 pValue .>> sstr1 "@" .>>. pValue) .>> skipNewline .>>. increaseIndent (pSubItems subitems)
      |>> fun ((account, ((q, c), price)), ss) -> let lns = ss |> List.tryPick (function SLotNames xs -> Some xs | _ -> None)
                                                               |> Option.defaultValue []
                                                  let reference = ss |> List.tryPick (function SReference r -> Some r | _ -> None)
                                                  let s = ss |> List.tryPick (function SAccount ("settlement", d) -> Some d | _ -> None)
                                                  let cg = ss |> List.tryPick (function SAccount ("cg", d) -> Some d | _ -> None)
                                                  let expenses = ss |> List.choose (function SExpensePosting (a,b,c) -> Some (a,b,c) | _ -> None)
                                                  let notes = ss |> List.choose (function SNote s -> Some s | _ -> None)
                                                  Trade {Account = account
                                                         Settlement = s
                                                         CapitalGains = cg
                                                         Asset = c
                                                         Quantity = q
                                                         Keff = 1.0M
                                                         PerUnitPrice = price
                                                         LotName = lns
                                                         Reference = reference
                                                         Expenses = expenses
                                                         Note = notes}

  choice [pComment; pAssertion; pSplit; pDividend; pTrade; pTransfer]

let pItem = tuple3 (p1 psequence)
                   ((opt (p1 (charReturn '*' true))) |>> function Some true -> true | _ -> false)
                   pElement
              |>> Item

let pPrices =
  let entry = (pnumber |>> Some) <|> (sstr "..." >>. preturn None) .>> nSpaces0
  let subitems = (p1 pdate .>>. many entry .>> skipNewline) |> indented |> many
  let expand (dt: DateTime, vals: decimal option list) = 
    match vals with 
      | [x] -> [(dt, x)]
      | xs  -> let ds = [for i in [0..List.length xs - 1] do yield dt.AddDays (float i)]
               List.zip ds xs
      |> List.choose (fun (a, b) -> match b with None -> None | Some x -> Some (a, x))
  sstr1 "prices" >>. p1 pCommodity .>>. pCommodity .>> skipNewline .>>. increaseIndent subitems
    |>> fun ((c, m), xs) -> Prices (commodity = c, measure = m, xs = List.collect expand xs)

let pRJournal =
  let parsers = [
    pIndent; pStartRegion; pEndRegion; pComment1;
    pHeader; pImport; pAccountDecl; pCommodityDecl;
    pItem; pPrices
  ]
  spaces >>. many (getPosition .>>. choice parsers .>> skipMany newline) .>> eof

let loadRJournal filename =
  runParser pRJournal UserState.Default (FromFile filename)