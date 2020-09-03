module Types

type Command =
  // Application management
  | Quit
  | Clear
  // File management
  | Load of string
  | Reload
  // Accounting
  | Balances of string