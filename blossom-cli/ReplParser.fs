module ReplParser

open System
open FParsec

open Types
open FilterParser

type Parser<'t> = Parser<'t, unit>

let ws = spaces
let ws1 = spaces1
let str = pstring

let pbool =
  choice
    [
      choice [for t in ["true"; "t"; "+"; "yes"; "y"; "1"] -> str t] >>. preturn true
      choice [for f in ["false"; "f"; "-"; "no"; "n"; "0"] -> str f] >>. preturn false
    ]

let pTenor : Parser<Tenor, unit>  =
  let z = anyOf "YMHQ"
           |>> function | 'Y' -> Y
                        | 'M' -> M
                        | 'H' -> Tenor.H
                        | 'Q' -> Q
                        | _   -> Y  // which shouldn't happen by design
  let w = pchar 'W' >>. opt pint32 |>> function Some i -> W (enum i) | _ -> W DayOfWeek.Friday
  (w <|> z)

// Command Parsing
// Flags
type Flag = GroupToTop | HideZeros | Flex | ShowVirtual | Cumulative | FlaggedOnly

let pFlag (fs: string) : Parser<Flag> =
  let flags =
    [
      'g', stringReturn "g" GroupToTop
      'z', stringReturn "z" HideZeros
      'x', stringReturn "x" Flex
      'v', stringReturn "v" ShowVirtual
      'c', stringReturn "c" Cumulative
      'f', stringReturn "f" FlaggedOnly
    ]
  flags |> List.choose (fun (c,p) -> if Seq.contains c fs then Some p else None)
        |> choice

let pFlags fs =
  (skipChar '-' >>. (many1 (pFlag fs) .>> followedBy ((skipAnyOf fs) <|> ws1 <|> eof)))
      <|>% []

let flagged xs v = List.contains v xs

// Application Management
let quit = choice [str "quit"; str ":q"] >>. preturn Quit
let clear = str "cls" >>. preturn Clear
let set =
  let gpv = str "performance_reporting" >>. ws1 >>. pbool |>> GPerformanceReporting
  str "set" >>. opt (ws1 >>. choice [gpv]) |>> Set

// File management
let load = choice [str "load"; str ":l"] >>. ws1 >>. restOfLine false |>> Load
let reload = choice [str "reload"; str ":r"] >>. preturn Reload

// Accounting
let balances =
  choice [str "balances"; str "bal"; str ":b"]
    >>. ws
    >>. pFlags "gzxv"
    .>>. pFilter
    |>> fun (fs, f) ->
          let br = {GroupToTop = flagged fs GroupToTop
                    HideZeros = flagged fs HideZeros
                    Flex = flagged fs Flex
                    ShowVirtual = flagged fs ShowVirtual}
          Balances (f, br)

let journal =
  choice [str "journal"; str ":j"]
    >>. ws
    >>. pFlags "zfxv"
    .>>. pFilter
    |>> fun (fs, f) ->
          let fr = {HideZeros = flagged fs HideZeros
                    FlaggedOnly = flagged fs FlaggedOnly
                    Flex = flagged fs Flex
                    ShowVirtual = flagged fs ShowVirtual}
          Journal (f, fr)

let series =
  choice [str "series"; str ":s"]
    >>. ws1
    >>. pTenor
    .>> ws
    .>>. pFlags "gzxvc"
    .>>. pFilter
    |>> fun ((t, fs), f) ->
          let sr = {GroupToTop = flagged fs GroupToTop
                    HideZeros = flagged fs HideZeros
                    Flex = flagged fs Flex
                    ShowVirtual = flagged fs ShowVirtual
                    Cumulative = flagged fs Cumulative
                    Tenor = t}
          BalanceSeries (f, sr)

// Investment
let lotAnalysis =
  let lr0 = {Measures = []}
  choice [str "lots"; str ":lt"]
   >>. ws
   >>. pFilter
   |>> fun f -> LotAnalysis (f, lr0)

// Help
let check = choice [str "check"; str ":c"] >>. ws1 >>. choice [str "assertions" >>. preturn Assertions] |>> Check
let help = str "help" >>. preturn Help

// Meta
let meta = str "meta" >>. ws1 >>.
            choice [stringReturn "statistics"  MetaRequestType.Statistics
                    stringReturn "accounts"    Accounts
                    stringReturn "commodities" Commodities
                    stringReturn "payees"      Payees
                    stringReturn "hashtags"    HashTags]
            |>> fun m -> Meta {RequestType = m; Regex = None}

let applicationCommands = [quit; clear; set]
let fileCommands = [load; reload;]
let accountingCommands = [balances; journal; series]
let investmentCommands = [lotAnalysis]
let otherCommands = [meta; check; help]
let parse : Parser<Command> =
  let commands = [applicationCommands; fileCommands; accountingCommands; investmentCommands; otherCommands] |> List.concat
  choice commands .>> eof