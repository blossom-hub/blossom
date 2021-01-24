module ReplParser

open System
open FParsec

open Types
open SubParsers

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

// Can make this clever by doing better validation, accepting more formats etc.
// let pdate = tuple3 (pint32 .>> pchar '-') (pint32 .>> pchar '-') pint32 |>> DateTime

let ws = spaces
let ws1 = spaces1
let str = pstring

let pbool =
  choice
    [
      choice [str "true"; str "t"; str "+"; str "yes"; str "y"; str "1"] >>. preturn true
      choice [str "false"; str "f"; str "-"; str "no"; str "n"; str "0"] >>. preturn false
    ]

// Command Parsing
// Flags'n'args
let flags : Parser<flags, unit> =
  let flg = pchar '-' >>. manyChars (asciiLower <|> digit) .>> ws
  many flg |>> Set.ofList

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
    >>. flags
    .>> ws
    .>>. restOfLine false
    |>> Balances
let journal = choice [str "journal"; str ":j"] >>. ws >>. flags .>> ws .>>. restOfLine false |>> Journal
let series =
  let pCumulative = opt (pchar '+') |>> function Some '+' -> true | _ -> false
  choice [str "series"; str ":s"] >>. ws1 >>.
    tuple4 (ws >>. flags) pTenor pCumulative (ws >>. restOfLine false) |>> BalanceSeries

// Help
let check = choice [str "check"; str ":c"] >>. ws1 >>. choice [str "assertions" >>. preturn Assertions] |>> Check
let help = str "help" >>. preturn Help

// Meta
let meta = str "meta" >>. ws1 >>.
            choice [str "stats" >>. preturn MetaRequest.Statistics
                    str "accounts" >>. preturn Accounts
                    str "commodities" >>. preturn Commodities
                    str "payees" >>. preturn Payees
                    str "hashtags" >>. preturn HashTags] |>> Meta

let applicationCommands = [quit; clear; set]
let fileCommands = [load; reload;]
let accountingCommands = [balances; journal; series]
let otherCommands = [meta; check; help]
let parse : Parser<Command> =
  let commands = [applicationCommands; fileCommands; accountingCommands; otherCommands] |> List.concat
  choice commands .>> eof