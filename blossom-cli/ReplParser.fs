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

// Command Parsing
// Flags'n'args
let flags = opt (skipString "-" >>. many asciiLower) |>> Option.defaultValue []

// Application Management
let quit = choice [str "quit"; str ":q"] >>. preturn Quit
let clear = str "cls" >>. preturn Clear

// File management
let load = choice [str "load"; str ":l"] >>. ws1 >>. restOfLine false |>> Load
let reload = choice [str "reload"; str ":r"] >>. preturn Reload

// Accounting
let balances = choice [str "balances"; str "bal"; str ":b"] >>. ws >>. flags .>>. restOfLine false |>> Balances
let journal = choice [str "journal"; str ":j"] >>. ws >>. restOfLine false |>> Journal
let series =
  let pCumulative = opt (pchar '+') |>> function Some '+' -> true | _ -> false
  choice [str "series"; str ":s"] >>. ws1 >>.
    tuple3 pTenor pCumulative (ws >>. restOfLine false) |>> BalanceSeries

// Help
let check = choice [str "check"; str ":c"] >>. ws1 >>. choice [str "assertions" >>. preturn Assertions] |>> Check

// Meta
let meta = str "meta" >>. ws1 >>. choice [str "accounts" >>. preturn Accounts
                                          str "commodities" >>. preturn Commodities
                                          str "payees" >>. preturn Payees
                                          str "hashtags" >>. preturn HashTags] |>> Meta

let applicationCommands = [quit; clear]
let fileCommands = [load; reload;]
let accountingCommands = [balances; journal; series]
let otherCommands = [meta; check]
let parse : Parser<Command> =
  let commands = [applicationCommands; fileCommands; accountingCommands; otherCommands] |> List.concat
  choice commands .>> eof