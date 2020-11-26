# Blossom Valuation Model

Blossom is by design a multi-currency, multi-asset platform, and with this comes complexity in the valuation of accounts _outside_ of the original denominations of the transactions inside those accounts. This document highlights the approach taken to ensure a consistent and logic result when converted and reporting.

> Ideas, thoughts, implementations etc. described on this page are undergoing design and specification, and are subject to revision.

### Motivating examples
Consider the following set of transactions
```
2015-01-20 Salary
  Income:Salary     -130000 JPY
  Assets:Banks:JPY

...

2020-01-20 Salary
  Income:Salary     -10000 HKD
  Assets:Banks:HKD

2020-01-22 Freelance remote work
  Income:Salary     -1000 USD
  Assets:Banks:USD
```
How do we compare these sorts of balances properly over time? What if we want to view them in a base currency of `USD`? On the other hand, the valuation _now_ of these assets in the bank accounts is something different - we can readily convert the currencies at the prevailing exchange rate as of any date which these exist. We've made a gain/loss with respect to this other currency.

We can thus consider that if I am paid in 2015 a salary in JPY, and hold the balance and the JPY strengthens, then I have an _unrealised gain_ in `USD` terms (my JPY balance has remained the same).

There is also a problem with trading over time with respect to expenses (for example)
```
2020-01-04 Buy Softbank 9984JP
  Assets:Brokerage:Stocks:JP    2000 9984JP @ 6500 JPY
  Expenses:Commission           1000 JPY  ; at 125 this is 8 USD
  Assets:Brokerage:Cash

2020-01-06 Sell Softbank 9984JP
  Assets:Brokerage:Stocks:JP    -1000 9984JP @ 6950 JPY
  Expenses:Commission             500 JPY   ; at 135 this is 3.7 USD
  Assets:Brokerage:Cash         6949500 JPY
  Income:CapitalGains

prices 9984JP JPY
  2020-01-04 6450
  2020-01-05 6550
  2020-01-06 7100

prices USD JPY
  2020-01-04 125
  2020-01-05 130
  2020-01-06 135
  ...
  2020-12-31 102
```

Our trading expenses in `USD` terms are fixed at the prevailing exchange rate that the expense was incurred. The latest rates available have no bearing on the `USD` valuation of those commissions in the past (and revaluing them at the prevailing exchange rate today is meaningless, there is no unrealised gain like on the asset account).

### Summary
We can thus summarise some key items.
1. Assets held over time accure _different_ unrealised gains if they are valued in different currencies (the same applies for liabilities and equity).
1. Asset/Liability/Equity accounts should therefore be valued upon aggregation at the prevailing exchange rate that applies to the date of aggregation. This means that balances grouped on a monthly basis should be grouped _then_ converted at the historical rates. "Today"s balances would thus use the latest exchange rates available.
1. Income/Expense accounts on the other hand are fixed in time for other denominations at the exchange rate at occurance (approximated with the end of day rate in most cases).
1. Tradable assets are thus required to track both their own price movements but also their movement with respect to the specified exchange rate.

It can be seen that balances sheet accounts are typically "latest" or "prevailing" accounted for, whilst income/expense style accounts use mostly "historical" or "point in type" exchange rates.

## Representation
It is possibly to _overly_ unify the above examples into a common format that would typically require more data than is necessary, as not all conversions are required, but nevertheless is a good view of the overall situation.

Given a wish to value everything in USD
- `ti` being the _spot_ transaction _time_
- `te` being the transaction date
- `T` being the valuation date (e.g. today)


The following table shows how we _could_ enrich the original transaction information (note - only changes shown for the right hand columns, end of day fx rates in all cases, for balances on those dates, not present value of tradables).

| Date | Description | Account | Quantity | | Commodity | Denomination | FX_te | Valuation_ti | Valuation USD_ti | Valuation_te | Valuation USD_te | FX_T | Valuation USD_T |
|- |- |- |- |- |- |- |- |- |- |- |- |- |- |
| 2015-01-22 | Salary | Income:Salary | -130,000 | | JPY | JPY | 150 | -130,000 | -866.67 | | | 104 | ~~-1,250~~|
| |  | Asset:Banks:JPY | | 130,000 | JPY | JPY | 150 | 130,000 | 866.67 | | | 104 | 1,250 |
| 2020-01-04 | Buy Softbank | Assets:Brokerage:Stocks:JP | | 2,000 | 9984JP | JPY | 125 |13,000,000 | 104,000 | 12,900,000 | 103,200 | 104 | ~~124,038~~ |
| |  | Expenses:Commission | | 1,000 | JPY | JPY | 125 | 1,000 | 8 | | | 104 | ~~9.615~~ |
| |  | Assets:Brokerage:Cash | -13,001,000 | | JPY | JPY | 125 | -13,001,000 | -104,008 | | | 104 | 125,009.62 |
| 2020-01-06 | Sell Softbank | Assets:Brokerage:Stocks:JP | -1,000 | | 9984JP | JPY | 135 | -6,950,000 | -51,481.48 | 0 | 0 | 104 | 0 |
| |  | Expenses:Commission | | 500 | JPY | JPY | 135 | 500 | 3.70 | | | 104 | ~~4.81~~ |
| |  | Assets:Brokerage:Cash | |  6,949,500 | JPY | JPY | 135 | 6,949,500 | 51,477.77 | | | 104 | 66,822.12 |
| 2020-01-20 | Salary | Income:Salary | -10,000 | | HKD | HKD | 7.85 | -10,000 | -1,273.88 | | | 7.8 | ~~-1,282.05~~ |
| |  | Asset:Banks:HKD | | 10,000 | HKD | HKD | 7.85 | 10,000 | 1,273.88 | | | 7.8 | 1,282.05 |
| 2020-01-22 | Freelance remote work | Income:Salary | -1,000 | | USD | USD | 1 | -1,000 | | | | 1 | ~~-1,000~~ |
| |  | Asset:Banks:USD | | 1,000 | USD | USD | 1| 1,000 | | | | 1 | 1,000 |