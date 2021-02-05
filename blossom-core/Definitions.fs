module Definitions

open Types

let marketAccount = Account "_Market"

let conversionsAccount = Account "_Conversions"

let internalDefaultCommodity = Commodity "$"  // this is not a parsable value

let getAccountConventionStubs convention =
  let core = ["Asset"; "Liability"; "Equity"; "Income"; "Expense"]
  match convention with
    | Financial5 -> core
    | Financial7 -> core @ ["Payable"; "Receivable"]
