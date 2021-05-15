module Types

open Types

type GlobalOptionValue =
  | GPerformanceReporting of bool
  | GFilterDebug of bool
  | GLoadTracing of bool

type Command =
  // Application management
  | Quit
  | Clear
  | Set of GlobalOptionValue option
  // File management
  | Load of string
  | Reload
  | Output of string
  // Accounting
  | Balances of Filter * BalancesRequest
  | Journal of Filter * JournalRequest
  | BalanceSeries of Filter * SeriesRequest
  // Investing
  | LotAnalysis of Filter * LotRequest
  | HoldingsAnalysis of Filter
  // Help
  | Check of CheckRequest
  | Help
  // Meta
  | Meta of MetaRequest