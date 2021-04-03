module Definitions

open Types

let capitalGainsAccount = Account ("_CapitalGains", None)
let dividendsAccount = Account ("_Dividends", None)

let marketAccount = Account ("_Market", None)

let conversionsAccount = Account ("_Conversions", None)

let internalDefaultCommodity = Commodity "$"  // this is not a parsable value

let getAccountConventionStubs convention =
  let core = ["Asset"; "Liability"; "Equity"; "Income"; "Expense"]
  match convention with
    | Financial5 -> core
    | Financial7 -> core @ ["Payable"; "Receivable"]
