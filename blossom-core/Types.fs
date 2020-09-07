module Types

// "NewType" style definitions
type Commodity = Commodity of string
type Account = Account of string
type AccountHierarchy = AccountHierarchy of Account list

type Value = Value of decimal * Commodity

type Amount =
  | V of Value
  | P of Value * Value

type CommodityClass =
  | Currency
  | Equity
  | Option
  | Future