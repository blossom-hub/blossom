module Types

type Command =
  // Application management
  | Quit
  | Clear
  // File management
  | Load of string
  | Reload
  | PrettyPrint of string
  // Accounting
  | Balances of string