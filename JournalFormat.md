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
100 MSFT @ 160 USD      ; make a trade "buy 100 Microsft at 160 USD / share"
-100 MSFT @ 180 USD     ; negative example (sale)
100 MSFT @ 166          ; shorthand if the commodity has a measure defined already
100 GBP -> 134 USD      ; convert one commodity to another (no trade)
100 GBP <- 134 USD      ;   the account at the end of the arrow will be increased by that value

```
- all commodities are textural, following the value (some punctuation allowed, no spaces)
- there is no symbolic ($) handling, e.g. `$100` is invalid
- an un-measured amount is an error if there is no global or account commodity in scope

### Dates
```
{yyyy-MM-dd}    ; 2020-01-23, 2021-12-25 etc
```

## Journal file elements
A Journal file is a list of elements. The journal file is whitespace sensitive, and should not contain tabs.

### Operational elements
#### Set indent size
````
.indent 4    ; default 2
````
- blossom is not an exercise in complex parser writing, so fix the indent size here
- per file (it is possible to overwrite this in the same file)

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
  note {text}                 ; (optional) free text
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
  underlying {commodity}                  ; (optional) underlying of this derivative commodity
  class {Currency|Equity|Option|Future}   ; (optional) provides better treatment
  multiplier {factor}                     ; (optional) for leveraged instruments
  mtm                                     ; (optional) mark as mark-to-market treatment
                                          ; note: this is default for Futures
```

### Transaction elements
- a double space is needed between an account tree and the amount

_Basic syntax_
- there can only be one missing (elided) amount
```
{date} {payee} | {narrative}      ; payee is optional, exclude | if not using
  {account tree one}  {amount}
  {account tree two}
```
A transaction can be flagged for review by adding a * after the date:
```
{date} * {payee} | {narrative}      ; flagged for later
  {account tree one}  {amount}
  {account tree two}
```

It is possible to override the related account for a transaction entry, so that two or more accounts can be used to balance the entry. This allows for finer grained control and better narratives for complex transactions. It is possible for an entry to settle to it's own account (common for conversions and trading).

The syntax is:
```
... ~               ; settle to same account
... ~{account}      ; settle to specified {account}
```

Consider these three entries:
```
2020-03-04 Buy MSFT
  Assets:Brokerage      100 MSFT @ 166 ~Assets:Cash
  Expenses:Brokerage      9.99 USD
  Assets:Operating Cash

2020-03-04 Buy MSFT
  Assets:Brokerage      100 MSFT @ 166 ~
  Expenses:Brokerage      9.99 USD ~Assets:Operating Cash

2020-03-05 Buy MSFT
  Assets:Brokerage      100 MSFT @ 166
  Expenses:Brokerage       9.99 USD
  Assets:Cash
```
1. `Assets:Brokerage` becomes long 100 MSFT and `Assets:Cash` is short 16,000 USD, whilst `Assets:Operating Cash` is short 9.99 USD.
1. `Assets:Brokerage` becomes long 100 MSFT and short 16,000 USD, whilst `Assets:Operating Cash` is short 9.99 USD.
2. `Assets:Brokerage` becomes long 100 MSFT and `Assets:Cash` become short 16009.99 USD.

_(provided a commodity declaration has been done for MSFT)_

This also means that the following is _technically_ correct and equivalent, but not necessarily preferred.
```
2020-03-05 Wendys | Buy Lunch
  Expenses:Fast food    12.99 USD ~Assets:Wallet

2020-03-05 Wendys | Buy Lunch
  Expenses:Fast food    12.99 USD
  Assets:Wallet
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
