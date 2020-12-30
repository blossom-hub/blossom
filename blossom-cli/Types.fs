module Types

open Types

type flags = Set<string>

type Command =
  // Application management
  | Quit
  | Clear
  // File management
  | Load of string
  | Reload
  // Accounting
  | Balances of flags * string
  | Journal of flags * string
  | BalanceSeries of flags * Tenor * bool * string
  // Help
  | Check of CheckRequest
  | Help
  // Meta
  | Meta of MetaRequest