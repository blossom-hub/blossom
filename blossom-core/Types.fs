module Types

open System

// "NewType" style definitions
type Comment = Comment of string
type Commodity = Commodity of string
type Account = Account of string
type AccountHierarchy = AccountHierarchy of Account list

type Value = decimal * Commodity

type Tenor = Y | M | H | Q | W of DayOfWeek | D

type Amount =
  | V of Value
  | T of Value * Value
  | X of Value * Value

type ValuationMode =
  | Latest
  | Historical

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
  hashtags: string Set
}

type JournalMeta = {
  Name: string
  Commodity: Commodity option
  CapitalGains: Account option
  Note: string option
}

type AccountDecl = {
  Account: Account
  ValuationMode: ValuationMode
  Commodity: Commodity option
  CapitalGains: Account option
  Note: string option
  Propagate: bool
}

type CommodityDecl = {
  Symbol: Commodity
  Measure: Commodity option
  Underlying: Commodity option
  Name: string option
  Klass: CommodityClass option
  Multiplier: decimal option
  Mtm: bool
}

type Entry = {
  Flagged : bool
  Date: DateTime
  Payee: string option
  Narrative: string
  HashTags: string Set
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

type CheckRequest = Assertions

type MetaRequest = Statistics | Accounts | Commodities | Payees | HashTags
type MetaStatistics = {
  Range: DateTime * DateTime
  Transactions: int * int
  Accounts: int
  Commodities: int
  Payees: int
  Hashtags: int
  Assertions: int
  Prices: int
}
type MetaResult =
  | Statistics of MetaStatistics
  | MetaResultSet of string Set