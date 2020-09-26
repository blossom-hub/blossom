module Types

open System

// "NewType" style definitions
type Commodity = Commodity of string
type Account = Account of string
type AccountHierarchy = AccountHierarchy of Account list

type Value = decimal * Commodity

type Tenor = Y | M | H | Q | W of DayOfWeek | D

type Amount =
  | V of Value
  | T of Value * Value
  | X of Value * Value

type CommodityClass =
  | Currency
  | Equity
  | Option
  | Future

type Filter = {
  flexmode: bool
  between: ((bool * DateTime) option * (bool * DateTime) option) option
  account: string option
  payee: string option
  narrative: string option
  commodity: string option
}

type JournalMeta = {
  Name: string
  Commodity: Commodity option
  CapitalGains: Account option
  Note: string option
}

type AccountDecl = {
  Account: Account
  Commodity: Commodity option
  CapitalGains: Account option
  Note: string option
}

type CommodityDecl = {
  Symbol: Commodity
  Measure: Commodity option
  Name: string option
  Klass: CommodityClass option
  Multiplier: decimal option
  Mtm: bool
}

type Entry = {
  Date: DateTime
  Payee: string option
  Narrative: string
  Postings: (Account * Amount * Account option) list
}

type Journal = {
  Meta: JournalMeta
  AccountDecls: Map<Account, AccountDecl>
  CommodityDecls: Map<Commodity, CommodityDecl>
  Register: Map<DateTime, Entry list>
  Prices: Map<Commodity * Commodity, Map<DateTime, decimal>>
  Splits: Map<Commodity, (DateTime * int * int) list>
  Assertions: (DateTime * Account * Value) list
}

type MetaRequest = Accounts | Commodities | Payees