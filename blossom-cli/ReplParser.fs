module ReplParser

open System
open FParsec

open Types
open FilterParser

type Parser<'t> = Parser<'t, unit>

let ws = spaces
let ws1 = spaces1
let str = pstring

let pdate = tuple3 (pint32 .>> pchar '-') (pint32 .>> pchar '-') pint32 |>> DateTime

let pbool =
  choice
    [
      choice [for t in ["true"; "t"; "+"; "yes"; "y"; "1"] -> str t] >>. preturn true
      choice [for f in ["false"; "f"; "-"; "no"; "n"; "0"] -> str f] >>. preturn false
    ]

let pTenor : Parser<Tenor, unit>  =
  let z = anyOf "YMHQ"
           |>> function | 'Y' -> Y
                        | 'M' -> Tenor.M
                        | 'H' -> Tenor.H
                        | 'Q' -> Q
                        | _   -> Y  // which shouldn't happen by design
  let w = pchar 'W' >>. opt pint32 |>> function Some i -> W (enum i) | _ -> W DayOfWeek.Friday
  (w <|> z)

// Command Parsing

type Flag = FS of char | FG of int

let flag c = pchar c |>> FS
let flagGroup = pchar 'g' >>. pint32 |>> FG

let pFlags0 flags = skipChar '-' >>. (many1 (choice flags)) .>> ws <|>% []
let pFlags (fs: string) = fs |> Seq.map flag |> pFlags0


let pValuationMeasure =
  let pCommodity =
    let first = letter <|> digit <|> anyOf "."
    many1Chars2 first (letter <|> digit <|> anyOf ".:-()_") |>> Commodity
  skipChar '=' >>. pCommodity

let flagged xs v = List.contains (FS v) xs
let getGroupingFlag = List.tryPick (function | FG n -> Some n | _ -> None)

// Application Management
let quit = choice [str "quit"; str ":q"] >>. preturn Quit
let clear = str "cls" >>. preturn Clear
let set =
  let gpv = str "performance_reporting" >>. ws1 >>. pbool |>> GPerformanceReporting
  let gfd = str "debug" >>. ws1 >>. pbool |>> GDebug
  let glt = str "load_tracing" >>. ws1 >>. pbool |>> GLoadTracing
  let vd  = str "valuation_date" >>. ws1 >>. opt (pdate <|> (stringReturn "today" DateTime.Today)) |>> GValuationDate
  let mr = str "max_rows" >>. ws1 >>. pint32 |>> GMaxRows
  str "set" >>. opt (ws1 >>. choice [gpv; gfd; glt; vd; mr]) |>> Set

// File management
let load = choice [str "load"; str ":l"] >>. ws1 >>. restOfLine false |>> Load
let reload = choice [str "reload"; str ":r"] >>. preturn Reload
let switch = str "switch" >>. opt (ws1 >>. pint32) |>> Switch
let close = str "close" >>. ws1 >>. pint32 |>> Close

// Accounting
let balances =
  choice [str "balances"; str "bal"; str ":b"]
    >>. ws
    >>. pFlags0 [flagGroup; flag 'z'; flag 'x'; flag 'v']
    .>>. opt pValuationMeasure
    .>>. pFilter
    |>> fun ((fs, vm), f) ->
          let br = {ValuationMeasure = vm
                    GroupingLevel = getGroupingFlag fs
                    ShowZeros = flagged fs 'z'
                    Flex = flagged fs 'x'
                    IncludeVirtual = flagged fs 'v'}
          Balances (f, br)

let journal =
  choice [str "journal"; str ":j"]
    >>. ws
    >>. pFlags "zfxv"
    .>>. pFilter
    |>> fun (fs, f) ->
          let fr = {ShowZeros = flagged fs 'z'
                    FlaggedOnly = flagged fs 'f'
                    Flex = flagged fs 'x'
                    IncludeVirtual = flagged fs 'v'}
          Journal (f, fr)

let series =
  choice [str "series"; str ":s"]
    >>. ws1
    >>. pTenor
    .>> ws
    .>>. pFlags0 [flagGroup; flag 'z'; flag 'x'; flag 'c'; flag 'v']
    .>>. pFilter
    |>> fun ((t, fs), f) ->
          let sr = {GroupingLevel = getGroupingFlag fs
                    ShowZeros = flagged fs 'z'
                    Flex = flagged fs 'x'
                    Cumulative = flagged fs 'c'
                    Tenor = t
                    IncludeVirtual = flagged fs 'v'}
          BalanceSeries (f, sr)

// Investment
let lotAnalysis =
  choice [str "lots"]
   >>. ws
   >>. pFlags "xogv"
   .>>. pFilter
   |>> fun (fs, f) ->
      let lr = { ClosedOnly = flagged fs 'x'
                 OpenOnly = flagged fs 'o'
                 Consolidated = flagged fs 'g'
                 IncludeVirtual = flagged fs 'v'}
      LotAnalysis (f, lr)

let holdingsAnalysis =
  choice [str "holdings"]
  >>. ws
  >>. pFlags "xv"
  .>>. pFilter
  |>> fun (fs, f) -> HoldingsAnalysis f

// Help
let check = choice [str "check"; str ":c"] >>. ws1 >>. choice [str "assertions" >>. preturn Assertions] |>> Check
let help = str "help" >>. preturn Help

// Meta
let meta = str "meta" >>. ws1 >>.
            choice [stringReturn "statistics"  MetaRequestType.Statistics
                    stringReturn "accounts"    Accounts
                    stringReturn "commodities" Commodities
                    stringReturn "payees"      Payees
                    stringReturn "tags"        Tags_]
            .>>. opt (ws1 >>. restOfLine false)
            |>> fun (m, r) -> Meta {RequestType = m; Regex = r}

let applicationCommands = [quit; clear; set]
let fileCommands = [load; reload; switch; close]
let accountingCommands = [balances; journal; series]
let investmentCommands = [lotAnalysis; holdingsAnalysis]
let otherCommands = [meta; check; help]
let parse : Parser<Command> =
  let commands = [applicationCommands; fileCommands; accountingCommands; investmentCommands; otherCommands] |> List.concat
  choice commands .>> eof