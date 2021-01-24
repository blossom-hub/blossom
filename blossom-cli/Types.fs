module Types

open Types

type flags = Set<string>

type GlobalOptionValue =
  | GPerformanceReporting of bool

type Command =
  // Application management
  | Quit
  | Clear
  | Set of GlobalOptionValue option
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