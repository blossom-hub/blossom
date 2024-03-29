module Types

open System

type SQ = DateTime * uint32 option

type AccountConvention = Financial5 | Financial7

// "NewType" style definitions
type Comment = Comment of string
type Commodity = Commodity of string
type LotName = AutoLotName of string | CustomLotName of string
type Account = Account of string | Account2 of string * string

type Value = decimal * Commodity
type XValue = Value * Value option

type Tenor = Y | M | H | Q | W of DayOfWeek | D

type LotType = Open | Extend | Reduce | Close

type ValuationMode =
  | Latest
  | Historical

type CommodityClass =
  | Currency
  | Equity
  | ETF
  | Option
  | Future
  | Fund
  | Other

type IssueLevel = 
  | Info
  | Warning
  | Error

type JournalMeta = {
  Name: string
  Commodity: Commodity option
  Note: string option
  Convention: AccountConvention option
  CapitalGains: Account option
  UnrealisedGains: Account option
}

type AccountDecl = {
  Account: Account
  ShortCode: int option
  Number: string option
  ValuationMode: ValuationMode
  Commodity: Commodity option
  Note: string option
}

type CommodityDecl = {
  Symbol: Commodity
  Measure: Commodity
  QuoteDP: int option
  Underlying: Commodity option
  Name: string option
  Klass: CommodityClass option
  Multiplier: decimal option
  Mtm: bool
  ExternalIdents: Map<string, string>
}

type ClosingTrade = {
  Date: SQ
  Settlement: Account
  CapitalGains: Account option
  Quantity: decimal
  Keff: decimal
  PerUnitPrice: decimal
  UnadjustedPnL: decimal
  LotName: string
  Reference: string option
  Expenses: (Account * Value * Account) list
}

type OpeningTrade = {
  Date: SQ
  Account: Account
  Settlement: Account
  Asset: Commodity
  Quantity: decimal
  OpenQuantity: decimal
  Keff: decimal
  Measure: Commodity
  PerUnitPrice: decimal
  LastUnitPrice: (DateTime * decimal)
  UnrealisedPnL: decimal
  LotName: string
  Reference: string option
  Expenses: (Account * Value * Account) list
  Closings: ClosingTrade list
}

type Entry = {
  Flagged : bool
  Automatic: bool
  Date: SQ
  Payee: string option
  Narrative: string
  Tags: string Set
  Postings: (Account * Value * Account) list
}

type Journal = {
  Meta: JournalMeta
  AccountDecls: Map<Account, AccountDecl>
  CommodityDecls: Map<Commodity, CommodityDecl>
  Register: Map<SQ, Entry list>
  InvestmentAnalysis: OpeningTrade list
  Prices: Map<Commodity * Commodity, Map<DateTime, decimal * decimal>>
  SplitKFactors: Map<Commodity, (DateTime * decimal) list>
  Assertions: (DateTime * Account * Value) list
  Issues: (IssueLevel * FParsec.Position * string) list
}

type Filter = {
  Timespan: Choice<int * int option * int option,
                   ((bool * DateTime) option * (bool * DateTime) option)> option
  Accounts: string list
  Payees: string list
  Narrative: string option
  Commodities: string list
  Measures: string list
  Tags: string list
}

type BalancesRequest = {
  ValuationMeasure: Commodity option
  GroupingLevel: int option
  ShowZeros: bool
  Flex: bool
  IncludeVirtual: bool
}

type JournalRequest = {
  ShowZeros: bool
  Flex: bool
  FlaggedOnly: bool
  IncludeVirtual: bool
}

type SeriesRequest = {
  GroupingLevel: int option
  ShowZeros: bool
  Flex: bool
  Cumulative: bool
  Tenor: Tenor
  IncludeVirtual: bool
}

type LotRequest = {
  Consolidated: bool
  OpenOnly: bool
  ClosedOnly: bool
  IncludeVirtual: bool
}

type CheckRequest = Assertions | Issues

type MetaRequestType = Statistics | Accounts | Commodities | Payees | Tags_
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
  Tags: int
  Assertions: int
  Prices: int
}
type MetaResult =
  | Statistics of MetaStatistics
  | MetaResultSet of string Set