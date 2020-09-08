module ReplParser

open System
open FParsec

open Types


type UserState = unit
type Parser<'t> = Parser<'t, UserState>

// Can make this clever by doing better validation, accepting more formats etc.
// let pdate = tuple3 (pint32 .>> pchar '-') (pint32 .>> pchar '-') pint32 |>> DateTime

let ws = spaces
let ws1 = spaces1
let str = pstring

// Command Parsing
// Application Management
let quit = choice [str "quit"; str ":q"] >>. preturn Quit
let clear = str "cls" >>. preturn Clear

// File management
let load = choice [str "load"; str ":l"] >>. ws1 >>. restOfLine false |>> Load
let reload = choice [str "reload"; str ":r"] >>. preturn Reload
let pprint = choice [str "pp"] >>. ws1 >>. restOfLine false |>> PrettyPrint

// Accounting
let balances = choice [str "balances"; str "bal"; str ":b"] >>. ws >>. restOfLine false |>> Balances

let applicationCommands = [quit; clear]
let fileCommands = [load; reload; pprint]
let accountingCommands = [balances]
let parse : Parser<Command> =
  let commands = applicationCommands @ fileCommands @ accountingCommands
  choice commands .>> eof