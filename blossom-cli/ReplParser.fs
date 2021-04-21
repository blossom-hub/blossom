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
                        | 'M' -> Tenor.M
                        | 'H' -> Tenor.H
                        | 'Q' -> Q
                        | _   -> Y  // which shouldn't happen by design
  let w = pchar 'W' >>. opt pint32 |>> function Some i -> W (enum i) | _ -> W DayOfWeek.Friday
  (w <|> z)

// Command Parsing

let pFlags (fs:string) =
  (skipChar '-' >>. (many1 (anyOf fs) .>> followedBy ((skipAnyOf fs) <|> ws1 <|> eof)))
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
          let br = {GroupToTop = flagged fs 'g'
                    HideZeros = flagged fs 'z'
                    Flex = flagged fs 'x'}
          Balances (f, br)

let journal =
  choice [str "journal"; str ":j"]
    >>. ws
    >>. pFlags "zfxv"
    .>>. pFilter
    |>> fun (fs, f) ->
          let fr = {HideZeros = flagged fs 'z'
                    FlaggedOnly = flagged fs 'f'
                    Flex = flagged fs 'x'}
          Journal (f, fr)

let series =
  choice [str "series"; str ":s"]
    >>. ws1
    >>. pTenor
    .>> ws
    .>>. pFlags "gzxvc"
    .>>. pFilter
    |>> fun ((t, fs), f) ->
          let sr = {GroupToTop = flagged fs 'g'
                    HideZeros = flagged fs 'z'
                    Flex = flagged fs 'x'
                    Cumulative = flagged fs 'c'
                    Tenor = t}
          BalanceSeries (f, sr)

// Investment
let lotAnalysis =
  choice [str "lots"]
   >>. ws
   >>. pFlags "xog"
   .>>. pFilter
   |>> fun (fs, f) ->
      let lr = { ClosedOnly = flagged fs 'x'
                 OpenOnly = flagged fs 'o'
                 Consolidated = flagged fs 'g'}
      LotAnalysis (f, lr)

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
            |>> fun m -> Meta {RequestType = m; Regex = None}

let applicationCommands = [quit; clear; set]
let fileCommands = [load; reload;]
let accountingCommands = [balances; journal; series]
let investmentCommands = [lotAnalysis]
let otherCommands = [meta; check; help]
let parse : Parser<Command> =
  let commands = [applicationCommands; fileCommands; accountingCommands; investmentCommands; otherCommands] |> List.concat
  choice commands .>> eof