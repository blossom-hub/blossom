module Types

open Types

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
  | Balances of Filter * BalancesRequest
  | Journal of Filter * JournalRequest
  | BalanceSeries of Filter * SeriesRequest
  // Investing
  | LotAnalysis of Filter * LotRequest
  // Help
  | Check of CheckRequest
  | Help
  // Meta
  | Meta of MetaRequest