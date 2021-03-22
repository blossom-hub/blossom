module Types

open System

type AccountConvention =  Financial5 | Financial7

// "NewType" style definitions
type Comment = Comment of string
type Commodity = Commodity of string
type LotName = AutoLotName of string | CustomLotName of string
type Account = | Account of string | VirtualisedAccount of string * string
type AccountHierarchy = AccountHierarchy of string list * string option

type Value = decimal * Commodity

type Tenor = Y | M | H | Q | W of DayOfWeek | D

type LotType = Open | Extend | Reduce | Close

type Amount =
  | V of Value
  | T of Value * Value * LotName List
  | X of Value * Value

type ValuationMode =
  | Latest
  | Historical

type CommodityClass =
  | Currency
  | Equity
  | Option
  | Future

type JournalMeta = {
  Name: string
  Commodity: Commodity option
  CapitalGains: Account option
  Note: string option
  Convention: AccountConvention option
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
  QuoteDP: int option
  Underlying: Commodity option
  Name: string option
  Klass: CommodityClass option
  Multiplier: decimal option
  Mtm: bool
  ExternalIdents: Map<string, string>
}

type Entry = {
  Flagged : bool
  Date: DateTime
  Payee: string option
  Narrative: string
  HashTags: string Set
  Postings: (Account * Amount * Account) list
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

type Filter = {
  Timespan: ((bool * DateTime) option * (bool * DateTime) option) option
  Accounts: string list
  Payees: string list
  Narrative: string option
  Commodities: string list
  Hashtags: string list
  VAccount: string option
}

type BalancesRequest = {
  GroupToTop: bool
  HideZeros: bool
  Flex: bool
  ShowVirtual: bool
}

type JournalRequest = {
  HideZeros: bool
  Flex: bool
  FlaggedOnly: bool
  ShowVirtual: bool
}

type SeriesRequest = {
  GroupToTop: bool
  HideZeros: bool
  Flex: bool
  ShowVirtual: bool
  Cumulative: bool
  Tenor: Tenor
}

type LotRequest = {
  Measures: string list
}

type CheckRequest = Assertions

type MetaRequestType = Statistics | Accounts | Commodities | Payees | HashTags
type MetaRequest = {
  RequestType: MetaRequestType
  Regex: string option
}

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