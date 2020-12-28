module Types

open Types

type flags = char list

type Command =
  // Application management
  | Quit
  | Clear
  // File management
  | Load of string
  | Reload
  // Accounting
  | Balances of flags * string
  | Journal of string
  | BalanceSeries of Tenor * bool * string
  // Help
  | Check of CheckRequest
  | Help
  // Meta
  | Meta of MetaRequest