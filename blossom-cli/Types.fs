module Types

open Types

type flags = string

type Command =
  // Application management
  | Quit
  | Clear
  // File management
  | Load of string
  | Reload
  // Accounting
  | Balances of flags list * string
  | Journal of string
  | BalanceSeries of Tenor * bool * string
  // Help
  | Check of CheckRequest
  | Help
  // Meta
  | Meta of MetaRequest