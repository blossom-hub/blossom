open System
open System.IO
open System.Text.RegularExpressions

let debug f v = printfn "%A" v; f v

let first f (a,b) = (f a, b)

// Roll my own independent journal parser, with the following passes
// 1. Localised basic syntax and primitives
// 2. Global semantic checks and validation steps

// https://stackoverflow.com/questions/33161244/in-f-is-it-possible-to-have-a-tryparse-function-that-infers-the-target-type

//type Loc = Loc of line:int * left:int * right:int
type L = L of line:int
type 'a Located = L * 'a

type 'a Parsed =
  | Success of 'a Located
  | Unrecognised of string Located

// primitive parsers
let inline tryParse ln text : ^a Parsed =
  let mutable r = Unchecked.defaultof<_>
  if (^a : (static member TryParse: string * ^a byref -> bool) (text, &r))
    then Success (ln, r)
    else Unrecognised (ln, text)

// imported from Types.fs
type SQ = SQ of DateTime * uint option
type AccountConvention = Financial5 | Financial7
type ValuationConvention = Latest | Historical
type CommodityClass =
  | Currency
  | Equity
  | ETF
  | Option
  | Future
  | Fund
  | Other
type Account = Account of string | Account2 of string * string

type Commodity = Commodity of string

type Value = decimal Parsed * Commodity Parsed


// LET'S GO
type HeaderElement =
  | HI_Commodity of Commodity Parsed
  | HI_Note of string
  | HI_Convention of AccountConvention Parsed
  | HI_CGA of Account Parsed
  | HI_URA of Account Parsed
  | HI_Comment of string

type AccountDeclElement =
  | AD_Number of string
  | AD_Commodity of Commodity Parsed
  | AD_Note of string
  | AD_Valuation of ValuationConvention Parsed
  | AD_ShortCode of uint Parsed
  | AD_Comment of string

type CommodityDeclElement =
  | CD_Name of string
  | CD_Note of string
  | CD_Measure of Commodity Parsed
  | CD_Class of CommodityClass Parsed
  | CD_ExternalIdent of string * string
  | CD_DP of uint Parsed
  | CD_Multiplier of uint Parsed
  | CD_Underlying of Commodity Parsed
  | CD_MTM
  | CD_Comment of string

type SubPriceElement =
  | SP_Price of decimal Parsed
  | SP_Skipped

type PriceElement =
  | PI_Sequence of DateTime Parsed * SubPriceElement Parsed list
  | PI_Comment of string

type DividendElement =
  | DE_Paydate of DateTime Parsed
  | DE_SettlementAccount of Account Parsed
  | DE_ReceivableAccount of Account Parsed
  | DE_IncomeAccount of Account Parsed
  | DE_Note of string
  | DE_Comment of string

type TradeElement =
  | TE_LotName of string
  | TE_Reference of string
  | TE_SettlementAccount of Account Parsed
  | TE_CGA of Account Parsed
  | TE_Expense of string // TODO
  | TE_Note of string
  | TE_Comment of string

type SubTransferElement =
  | ST_Split of Account Parsed * decimal Parsed
  | ST_Comment of string

type TransferElement =
  | TF_Note of string
  | TF_Tag of string
  | TF_Posting of Account Parsed * (decimal Parsed * Commodity Parsed * Account Parsed option) option * SubTransferElement Parsed list
  | TF_Comment of string

type JournalBlock =
  //
  | Indent of int
  | StartRegion of string
  | EndRegion
  | Comment of string
  //
  | Header of string * HeaderElement Parsed list
  | Import of string * string * JournalBlock Located seq option
  //
  | AccountDecl of Account Parsed * AccountDeclElement Parsed list
  | CommodityDecl of Commodity Parsed * CommodityDeclElement Parsed list
  //
  | Prices of Commodity Parsed * Commodity Parsed * PriceElement Parsed list
  //
  | Transfer of SQ * string * string option * TransferElement Parsed list
  | Trade of SQ * Account Parsed * Value * Value * TradeElement Parsed list
  | Dividend of SQ * Account Parsed * Value * Value * DividendElement Parsed list
  | Assertion of SQ * Account Parsed * Value
  | Split of SQ * Commodity Parsed * uint Parsed * uint Parsed
  //
  | Unknown of string Located list
  | Empty

let trim (s: string) = s.Trim()
let split (s: string) = s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

type SizedSplit =
  | SS_Success of string list * string option
  | SS_Fail of string list

let split2 n (s: string) =
  let xs = s.Split(' ', n+1,StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> Array.toList
  let nn = List.length xs
  match true with
    | _ when nn = n   -> SS_Success (xs, None)
    | _ when nn = n+1 -> SS_Success (xs[0..n-1], Some xs[n])
    | _ when nn < n   -> SS_Fail xs
    | _ -> failwith ".net string split fatal error"

let pop (input: string) =
  let xs = input.Split(' ', 2, StringSplitOptions.TrimEntries)
  match Array.length xs with
    | 0 -> ("", "")
    | 1 -> (input, "")
    | 2 -> (xs[0], xs[1])
    | _ -> failwith ".net string split fatal error"

let popIndent (line: string) =
  let indent = Seq.init line.Length (fun i -> line[i])
                |> Seq.takeWhile (fun c -> c = ' ')
                |> Seq.length
  indent, line[indent..]

let getIndent ((i, line): string Located) =
  Seq.init line.Length (fun i -> line[i])
  |> Seq.takeWhile (fun c -> c = ' ')
  |> Seq.length

let chunk1Level fn (lines: string Located seq) =
  if Seq.isEmpty lines
    then Seq.empty
    else let indent = getIndent (Seq.head lines)
         let blocks = lines |> Seq.fold (fun (blocks, currentBlock) (idx, line) ->
                                     match popIndent line with
                                       | (_, "")   -> (blocks, currentBlock)
                                       | (i, content) when i > indent -> (blocks, (idx, line[indent..])::currentBlock)
                                       | (i, content) -> ((List.rev currentBlock |> fn)::blocks, [idx, content]))
                                  ([], [])
         // collect the final block which won't trigger
         let final = snd blocks |> List.rev |> fn
         let blocks = final :: fst blocks
         // skip the first one which is going to be empty
         Seq.rev blocks |> Seq.skip 1

let createItem cmt fn expectedIndent ((i, line) : string Located) =
  let indent, content = popIndent line
  match true with
    | _ when List.contains content[0] [';'; '*'] -> trim content[1..] |> cmt |> fun x -> Success (i, x)
    | _ when indent <> expectedIndent            -> Unrecognised (i, line)
    | _ -> let a,b = pop content
           match fn (i, a, b) with Some r -> r | None -> Unrecognised (i, line)

// shortcut helper!
let ss i x = Some (Success (i, x))

// Regex driven active recognisers
let (|Sq|_|) (input: string) =
  let m = Regex.Match(input, @"((\d{4})-(\d{2})-(\d{2}))((\/)(\d+))?$")
  if m.Success
    then try
           let year = int m.Groups[2].Value
           let month = int m.Groups[3].Value
           let day = int m.Groups[4].Value
           let dt = DateTime(year, month, day)
           if m.Groups[7].Success
             then Some (dt, Some (uint m.Groups[7].Value))
             else Some (dt, None)
         with
           | _ -> None
    else None

let tryParseAccount0 (input: string) =
  let m = Regex.Match(input, @"^((?:[A-Z]\w*)(?::[A-Z\d]\w*)*)(?:\/(\w+))?$")
  if m.Success
    then Some <| if m.Groups[2].Success then Account2 (m.Groups[1].Value, m.Groups[2].Value)
                                        else Account m.Groups[1].Value
    else None
let (|PAccount|_|) = tryParseAccount0
let tryParseAccount i = function PAccount acct -> Success (i, acct) | v -> Unrecognised (i, v)

let tryParseCommodity i (input: string) =
  let m = Regex.Match(input, @"^[A-Za-z0-9][A-Za-z0-9._]*$")
  if m.Success
    then Success (i, Commodity input)
    else Unrecognised (i, input)

let createHeaderItem =
  createItem HI_Comment <|
    function | i, "note", v       -> ss i <| HI_Note v
             | i, "commodity", v  -> ss i <| HI_Commodity (tryParseCommodity i v)
             | i, "convention", v -> ss i <| HI_Convention (
                                        match v with
                                          | "F5" -> Success (i, Financial5)
                                          | "F7" -> Success (i, Financial7)
                                          | _    -> Unrecognised (i, v))
             | i, "cg", v         -> ss i <| HI_CGA (tryParseAccount i v)
             | i, "ug", v         -> ss i <| HI_URA (tryParseAccount i v)
             | _ -> None

let createAccountDecl =
  createItem AD_Comment <|
    function | i, "short", v     -> ss i <| AD_ShortCode (tryParse i v)
             | i, "number", v    -> ss i <| AD_Number v
             | i, "valuation", v -> ss i <| AD_Valuation (
                                      match v with
                                        | "Latest"     -> Success (i, Latest)
                                        | "Historical" -> Success (i, Historical)
                                        | _            -> Unrecognised (i, v))
             | i, "commodity", v -> ss i <| AD_Commodity (tryParseCommodity i v)
             | i, "note", v      -> ss i <| AD_Note v
             | _ -> None

let createCommodityDecl =
  createItem CD_Comment <|
    function | i, "measure", v    -> ss i <| CD_Measure (tryParseCommodity i v)
             | i, "dp", v         -> ss i <| CD_DP (tryParse i v)
             | i, "underlying", v -> ss i <| CD_Underlying (tryParseCommodity i v)
             | i, "name", v       -> ss i <| CD_Name v
             | i, "class", v      -> ss i <| CD_Class (
                                       match v with
                                         | "Currency" -> Success (i, Currency)
                                         | "Equity"   -> Success (i, Equity)
                                         | "ETF"      -> Success (i, ETF)
                                         | "Option"   -> Success (i, Option)
                                         | "Future"   -> Success (i, Future)
                                         | "Fund"     -> Success (i, Fund)
                                         | "Other"    -> Success (i, Other)
                                         | _          -> Unrecognised (i, v))
             | i, "multiplier", v -> ss i <| CD_Multiplier (tryParse i v)
             | i, "mtm", v        -> match v with "" -> ss i <| CD_MTM | _ -> None
             | i, "note", v       -> ss i <| CD_Note v
             | i, "externalid", v -> ss i <| CD_ExternalIdent (pop v)
             | _ -> None

let createDividendElement =
  createItem DE_Comment <|
    function | i, "paydate", (Sq (dt, None)) -> ss i <| DE_Paydate (Success (i, dt))
             | i, "paydate", v               -> ss i <| DE_Paydate (Unrecognised (i, v))
             | i, "settlement", v            -> ss i <| DE_SettlementAccount (tryParseAccount i v)
             | i, "receivable", v            -> ss i <| DE_ReceivableAccount (tryParseAccount i v)
             | i, "income", v                -> ss i <| DE_IncomeAccount (tryParseAccount i v)
             | i, "note", v                  -> ss i <| DE_Note v
             | _ -> None

let createTradeElement =
  createItem TE_Comment <|
    function | i, "lot", v        -> ss i <| TE_LotName v
             | i, "reference", v  -> ss i <| TE_Reference v
             | i, "settlement", v -> ss i <| TE_SettlementAccount (tryParseAccount i v)
             | i, "cg", v         -> ss i <| TE_CGA (tryParseAccount i v)
             | i, "expense", v    -> ss i <| TE_Expense v
             | i, "note", v       -> ss i <| TE_Note v
             | _ -> None

let createPricesElement =
  let fn i xs = split xs |> Seq.map (function | "..." -> Success (i, SP_Skipped) | v -> tryParse i v |> SP_Price |> fun p -> Success (i,p)) |> Seq.toList
  createItem PI_Comment <|
    function | i, Sq (dt, None), v -> ss i <| PI_Sequence (Success (i, dt), fn i v)
             | i, u, v             -> ss i <| PI_Sequence (Unrecognised (i, u), fn i v)
             | _ -> None

let createTransferElement chunk =
  let primary, children = match chunk with | [x] -> x, None | xs -> List.head xs, Some (List.tail xs)
  let cs = match children with
             | None -> []
             | Some xs -> let indent = getIndent xs[0]
                          xs |> List.map (fun x -> createItem ST_Comment (function | i, a, v when a[0] = '~' -> match split2 2 v with
                                                                                                                  | SS_Success (xs, trailing) -> ss i <| ST_Split (tryParseAccount i a[1..], tryParse i xs[0])
                                                                                                                  | SS_Fail [x]  -> ss i <| ST_Split (tryParseAccount i a[1..], tryParse i v)
                                                                                                                  | SS_Fail []   -> ss i <| ST_Split (tryParseAccount i a[1..], Unrecognised (i, v))
                                                                                                                  | _ -> None
                                                                                   | _ -> None)
                                                                         indent x)
  let p = createItem TF_Comment (function | i, "note", v      -> ss i <| TF_Note v
                                          | i, "tag", v       -> ss i <| TF_Tag v
                                          | i, PAccount d, v  -> match split2 2 v with
                                                                   | SS_Success (xs, Some trailing) ->
                                                                       match trailing[0] with
                                                                         | '~' ->  ss i <| TF_Posting (Success (i, d), Some (tryParse i xs[0], tryParseCommodity i xs[1], Some (tryParseAccount i trailing[1..])), cs)
                                                                         | _   ->  ss i <| TF_Posting (Success (i, d), Some (tryParse i xs[0], tryParseCommodity i xs[1], None), cs)
                                                                   | SS_Success (xs, None) -> ss i <| TF_Posting (Success (i, d), Some (tryParse i xs[0], tryParseCommodity i xs[1], None), cs)
                                                                   | SS_Fail []            -> ss i <| TF_Posting (Success (i, d), None, cs)
                                                                   | SS_Fail _ -> None
                                          | _ -> None  ) 0 primary
  p

let go children fn =
  match children with
    | [] -> []
    | xs -> xs |> List.map (fn (getIndent xs[0]))

let rec createSqElement sq (i, line) children : JournalBlock Located =
  let directive, content = pop line
  let result =
    match directive with
      | "*"     -> Some <| createSqElement sq (i, content) children
      | "split" -> match split2 3 content with
                     | SS_Success (xs, trailing) -> Some <| (i, Split (sq, tryParseCommodity i xs[0], tryParse i xs[1], tryParse i xs[2]))
                     | _ -> None
      | "assert" -> match split2 3 content with
                     | SS_Success (xs, trailing) -> Some <| (i, Assertion (sq, tryParseAccount i xs[0], (tryParse i xs[1], tryParseCommodity i xs[2])))
                     | _ -> None
      | "dividend" -> match split2 6 content with
                        | SS_Success (xs, trailing) -> Some <| (i, Dividend (sq, tryParseAccount i xs[0],
                                                                            (tryParse i xs[1], tryParseCommodity i xs[2]),
                                                                            (tryParse i xs[4], tryParseCommodity i xs[5]),
                                                                            go children createDividendElement))
                        | _ -> None
      | "trade" -> match split2 6 content with
                     | SS_Success (xs, trailing) -> Some <| (i, Trade (sq, tryParseAccount i xs[0],
                                                                      (tryParse i xs[1], tryParseCommodity i xs[2]),
                                                                      (tryParse i xs[4], tryParseCommodity i xs[5]),
                                                                      go children createTradeElement))
                     | _ -> None
      | _ -> // try a transfer element
             let header = line.Split('|', 2, StringSplitOptions.TrimEntries) |> Array.toList
             // these children may have 1 further layer of children to capture
             let cs = chunk1Level id children |> Seq.map createTransferElement |> Seq.toList
             match header with
               | [x]       -> Some <| (i, Transfer(sq, x, None, cs))
               | (x::y::_) -> Some <| (i, Transfer(sq, y, Some x, cs))
               | _         -> failwith ".net string split fatal error"

  match result with
    | Some r -> r
    | None   -> i, Unknown ((i, line) :: children)

let rec parseJournal standalone filename : JournalBlock Located seq =
  let createBlock (source: (L * string) list) =
    match source with
      | [] -> (L -1, Empty)
      | xs  ->
        let bsl = fst xs[0]
        if List.contains (snd xs[0]).[0] [';';'*'] then (bsl, Comment (trim (snd xs[0]).[1..]))
          else let ys, rest = pop (snd xs[0])
               bsl, match ys with
                      | ".indent"    -> int rest |> Indent
                      | "#region"    -> StartRegion rest
                      | "#endregion" -> EndRegion
                      //
                      | "journal"    -> Header (rest, go (List.tail xs) createHeaderItem)
                      | "import"     -> let resolved = if Path.IsPathRooted rest
                                                             then rest
                                                             else Path.Combine (Path.GetDirectoryName (filename: string), rest)
                                        if standalone
                                          then Import (rest, resolved, None)
                                          else Import (rest, resolved, parseJournal standalone resolved |> Some)
                      //
                      | "account"    -> AccountDecl (tryParseAccount bsl rest, go (List.tail xs) createAccountDecl)
                      | "commodity"  -> CommodityDecl (tryParseCommodity bsl rest, go (List.tail xs) createCommodityDecl)
                      //
                      | "prices"     -> match split2 2 rest with
                                          | SS_Success (zs, trailing) -> Prices(tryParseCommodity bsl zs[0], tryParseCommodity bsl zs[1], go (List.tail xs) createPricesElement)
                                          | _ -> Unknown xs
                      | Sq sq -> createSqElement (SQ sq) (bsl, rest) (List.tail xs) |> snd
                      | _ -> Unknown xs

  let lines = File.ReadAllLines filename |> Array.indexed |> Array.map (first L)
  chunk1Level createBlock lines


let test () =
  parseJournal true @"C:\Code\github\blossom\blossom-core\testdata\tester.fledge"

let test1 () =
  parseJournal true @"C:\Code\omnishark\finance\blossom\joanne_2021.fledge"

let test2 () =
  parseJournal false @"C:\Code\omnishark\finance\blossom\journal.fledge"


let findUnknowns xs =
  xs |> Seq.choose (function | Unknown _ as X -> Some X | _ -> None)


let rec findUnrecognised xs =
  xs |> Seq.collect (fun x ->
          match snd x with
            | Indent _
            | StartRegion _
            | EndRegion
            | Comment _
                -> Seq.empty
            | Header (_, hepl) ->
                hepl |> Seq.choose (function | Success (_, HI_Commodity (Unrecognised (ln, t))) -> Some (ln, t)
                                             | Success (_, HI_Convention (Unrecognised (ln, t))) -> Some (ln, t)
                                             | Success (_, HI_CGA (Unrecognised (ln, t))) -> Some (ln, t)
                                             | Success (_, HI_URA (Unrecognised (ln, t))) -> Some (ln, t)
                                             | Unrecognised (ln, t) -> Some (ln, t)
                                             | _ -> None)
            | Import (_, _, jbl) ->
                match jbl with | Some jbll -> findUnrecognised jbll | _ -> []
            | AccountDecl (acct, adepl) ->
                let a = match acct with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let b = adepl |> List.collect (function | Success (_, AD_Valuation (Unrecognised (ln, t))) -> [ln, t]
                                                        | Success (_, AD_Commodity (Unrecognised (ln, t))) -> [ln, t]
                                                        | Success (_, AD_ShortCode (Unrecognised (ln, t))) -> [ln, t]
                                                        | Unrecognised (ln, t) -> [ln, t]
                                                        | _ -> [])
                Seq.concat [a;b]
            | CommodityDecl (_, cdepl) ->
                cdepl |> Seq.choose (function | Success (_, CD_Measure (Unrecognised (ln, t))) -> Some (ln, t)
                                              | Success (_, CD_Class (Unrecognised (ln, t))) -> Some (ln, t)
                                              | Success (_, CD_DP (Unrecognised (ln, t))) -> Some (ln, t)
                                              | Success (_, CD_Underlying (Unrecognised (ln, t))) -> Some (ln, t)
                                              | Success (_, CD_Multiplier (Unrecognised (ln, t))) -> Some (ln, t)
                                              | Unrecognised (ln, t) -> Some (ln, t)
                                              | _ -> None)
            | Prices (c1, c2, pepl) ->
                let a = match c1 with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let b = match c2 with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let c = pepl |> List.collect (function | Success (_, PI_Sequence (a, qs)) -> let ys = qs |> List.choose (function | Success (_, SP_Price (Unrecognised (ln, t))) -> Some (ln, t)
                                                                                                                                  | Unrecognised (ln, t) -> Some (ln, t)
                                                                                                                                  | _ -> None)
                                                                                             match a with Unrecognised (ln, t) -> (ln, t)::ys | _ -> ys
                                                       | Unrecognised (ln, t) -> [ln, t]
                                                       | _ -> [])
                Seq.concat [a;b;c]
            | Transfer (_, _, _, tepl) ->
                tepl |> Seq.collect (function | Success (_, TF_Posting (acct, vopt, qs)) -> let a = match acct with Unrecognised (ln, t) -> [ln, t] | _ -> []
                                                                                            let b1 = match vopt with Some (Unrecognised (ln, t), _, _) -> [ln, t] | _ -> []
                                                                                            let b2 = match vopt with Some (_, Unrecognised (ln, t), _) -> [ln, t] | _ -> []
                                                                                            let b3 = match vopt with Some (_, _, Some (Unrecognised (ln, t))) -> [ln, t] | _ -> []
                                                                                            let c = qs |> List.collect (function | Success (_, ST_Split (a,t)) -> let e = match a with Unrecognised (ln, u) -> [ln, u] | _ -> []
                                                                                                                                                                  let f = match t with Unrecognised (ln, u) -> [ln, u] | _ -> []
                                                                                                                                                                  List.concat [e;f]
                                                                                                                                 | Unrecognised (ln, t) -> [ln, t]
                                                                                                                                 | _ -> [])
                                                                                            Seq.concat [a;b1;b2;b3;c]
                                              | Unrecognised (ln, t) -> [ln, t]
                                              | _ -> [])
            | Trade (_, acct, (qty, qc), (price,pc), tepl) ->
                let a = match acct with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let b = match qty with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let c = match qc with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let d = match price with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let e = match pc with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let f = tepl |> List.collect (function | Success (_, TE_SettlementAccount (Unrecognised (ln, t))) -> [ln, t]
                                                       | Success (_, TE_CGA (Unrecognised (ln, t))) -> [ln, t]
                                                       | Unrecognised (ln, t) -> [ln, t]
                                                       | _ -> [])
                Seq.concat [a;b;c;d;e;f]
            | Dividend (_, acct, (qty,qc), (puv, pc), depl) ->
                let a = match acct with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let b = match qty with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let c = match qc with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let d = match puv with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let e = match pc with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let f = depl |> List.collect (function | Success (_, DE_Paydate (Unrecognised (ln, t))) -> [ln, t]
                                                       | Success (_, DE_SettlementAccount (Unrecognised (ln, t))) -> [ln, t]
                                                       | Success (_, DE_ReceivableAccount (Unrecognised (ln, t))) -> [ln, t]
                                                       | Success (_, DE_IncomeAccount (Unrecognised (ln, t))) -> [ln, t]
                                                       | Unrecognised (ln, t) -> [ln, t]
                                                       | _ -> [])
                Seq.concat [a;b;c;d;e;f]
            | Assertion (_, acct, (qty, qc)) ->
                let a = match acct with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let b = match qty with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let c = match qc with Unrecognised (ln, t) -> [ln, t] | _ -> []
                Seq.concat [a;b;c]
            | Split (_, c1, k1, k2) ->
                let a = match c1 with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let b = match k1 with Unrecognised (ln, t) -> [ln, t] | _ -> []
                let c = match k2 with Unrecognised (ln, t) -> [ln, t] | _ -> []
                Seq.concat [a;b;c]
            | Unknown xs -> xs
            | Empty      -> Seq.empty
        )


// local syntax and conversion checks

type Error = Error of name:string * message:string * source:string Located
type Warning = Warning of name:string * message:string * source:string Located

type 'a Checked =
  | Okay of 'a
  | Errors of Error list

// yey fun, classic, functions ;-)
let merge a b = 
  match a, b with
    | Okay x, Okay y       -> Okay (x,y)
    | Errors xs, Okay _    -> Errors xs
    | Okay _, Errors ys    -> Errors ys
    | Errors xs, Errors ys -> Errors (List.concat [xs; ys])

let merge3 a b c = 
  match merge (merge a b) c with 
    | Okay ((x, y), z) -> Okay (x,y,z)
    | Errors xs        -> Errors xs

let merge4 a b c d = 
  match merge (merge3 a b c) d with 
    | Okay ((w, x, y), z) -> Okay (w,x,y,z)
    | Errors xs           -> Errors xs

let merge5 a b c d e = 
  match merge (merge4 a b c d) e with 
    | Okay ((v,w,x,y), z) -> Okay (v,w,x,y,z)
    | Errors xs           -> Errors xs

let merge6 a b c d e f = 
  match merge (merge5 a b c d e) f with 
    | Okay ((u,v,w,x,y), z) -> Okay (u,v,w,x,y,z)
    | Errors xs             -> Errors xs

let filterParsed p xs = 
  let ys = List.choose p xs
  match ys with
    | [] -> None, []
    | zs -> Some (List.head zs), List.tail zs

let checkUnrecognised label elt =
  match elt with | Unrecognised (ln, t) -> Errors [Error (label, "Unrecognised", (ln, t))]
                 | Success (_, ac)      -> Okay ac

let checkUnrecognisedOpt label elt =
  let elt1 = Option.map (checkUnrecognised label) elt
  match elt1 with | Some (Okay v)    -> Okay (Some v)
                  | Some (Errors es) -> Errors es
                  | None             -> Okay None

// TODO
let checkIsEmpty label elts : Option<List<Warning>> =
  match elts with 
    | []  -> None
    | xs  -> Some []

let pr1 x = Some x, []
let pr2 es = None, es
type 'a Result = { Item: 'a option; Errors: Error list; Warnings: Warning List}

type AccountDecl0 = {
  Account: Account
  ShortCode: uint option
  Number: string option
  ValuationMode: ValuationConvention
  Commodity: Commodity option
  Note: string option
}

let convertAccountDecl acct adepl =
  let number, numbers = filterParsed (function | (Success (_, AD_Number s)) -> Some s | _ -> None) adepl
  let note, notes = filterParsed (function | (Success (_, AD_Note s)) -> Some s | _ -> None) adepl
  let commodityP, commoditiesP = filterParsed (function | (Success (_, AD_Commodity s)) -> Some s | _ -> None) adepl
  let shortCodeP, shortCodesP = filterParsed (function | (Success (_, AD_ShortCode s)) -> Some s | _ -> None) adepl
  let valuationP, valuationsP = filterParsed (function | (Success (_, AD_Valuation s)) -> Some s | _ -> None) adepl

  let nameC = checkUnrecognised "Account name" acct
  let commodityC = checkUnrecognisedOpt "Account measure" commodityP 
  let shortCodeC = checkUnrecognisedOpt "Short code" shortCodeP 
  let valuationC = checkUnrecognisedOpt "Valuation convention" valuationP

  let merged = merge4 nameC commodityC shortCodeC valuationC
  let item, errors = 
    match merged with
      | Okay (name, commodity, shortCode, valuation) -> {Account = name
                                                         ShortCode = shortCode
                                                         Number = number
                                                         ValuationMode = valuation |> Option.defaultValue Latest
                                                         Commodity = commodity
                                                         Note = note } |> pr1
      | Errors es -> pr2 es

  // TODO
  let warnings = [
      checkIsEmpty "Account number" numbers
    ]
  {Item = item; Errors = errors; Warnings = warnings |> List.choose id |> List.concat}