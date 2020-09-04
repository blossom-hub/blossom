# Journal format

## Definitions
### Account trees
``Level1:Level Two:Level3``
- only a single space between words
- must be followed by a double space before use in a transaction element
### Amounts
```
100
100 USD
100 MSFT @ 160 USD      ; a "priced" amount
-100 MSFT @ 180 USD     ; negative example
```
- all commodities are textural,
- there is no symbolic ($) handling, e.g. `$100` is invalid
- all symbols follow the numerical portion
- the first works only if the account commodity or global commodity has been set
- the final entry is per-unit price of one of the first commodity (_1 share of MSFT is 160 USD_)

### Dates
```
{yyyy-MM-dd}    ; 2020-01-23, 2021-12-25 etc
```

## Journal file elements
A Journal file is a list of elements. The journal file is whitespace sensitive, and should not contain tabs.

### Operational element
#### Link to another file
```
import expenses_2019.dat
```
#### Comments
```
; rest of line comment
```
- comments are associated with the _preceeding_ element
  - _except_ if they are not indented, and then are associated with the _following_ element

### Declarations
- local items override global items (e.g. account commodity overrides journal commodity)
```
journal {name}                ;  [0..1] per file
  commodity {commodity}       ; (optional) default global commodity
  cg {account tree}           ; (optional) default capital gains account
```

```
account {account tree}        ; [0..] per file
  commodity {commodity}       ; (optional) default account commodity
  note {text}                 ; (optional) free text
  cg {account tree}           ; (optional) associated capital gains (overrides default)
```

```
commodity {commodity}                     ; [0..] per file
  name {text}                             ; (optional) full name
  measure {commodity}                     ; (optional) default measure [pricing] commodity
  class {Currency|Equity|Option|Future}   ; (optional) provides better treatment
  multiplier {factor}                     ; (optional) for leveraged instruments
  mtm                                     ; (optional) mark as mark-to-market treatment
                                          ; note: this is default for Futures
```

### Transaction elements
- a double space is needed between an account tree and the amount
- there can only be one missing (elided) amount
```
{date} {payee} | {narrative}      ; payee is optional, exclude | if not using
  {account tree one}  {amount}
  {account tree two}
```


### Prices and splits
```
prices {commodity} {unit commodity}
  {date} {value}
  {date} {value}
  ...

split {date} {commodity} {old units} {new units}
```

### Assertions
```
assert {date} {account tree}  {amount}
```
- must have double space after account tree
## Example
```
; give overall metadata for file, set default currency
journal Day-to-day 2020
  commodity USD
  cg Income:Capital Gains

; include previous year
import d2d_2019.dat

; pre-define some accounts
account Asset:Cash
  commodity USD
account Asset:Sofa Cash

; what did I do this year?

2020-01-01 KFC | New Year Chicken Bucket
  Expense:Food:Takeaway     18.99
  Asset:Cash

2020-01-05 Buy some Boeing
  Asset:Brokerage:Stonks     100 BA @ 160 USD
  Asset:Brokerage:Cash

2020-03-28 Sell BA !@#!#@!      ; oh... pandemic
  Asset:Brokerage:Stonks     -100 BA @ 1 USD
  Asset:Brokerage:Cash
```