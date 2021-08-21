# Journal format

> For most trading entry information, see the [Trading.md] file instead.

## Definitions
### Account trees
``Level1:LevelTwo:Level3``
- no spaces allowed (this is a change from earlier version)

``Level1:LevelTow:Level3/VirtualAccount``
- can have a "virtual account", which is dropped in most functions - but can be included using a flag
### Amounts

```
100 USD
```
- commodity (measures) should always be addded
- all commodities are textural, following the value (some punctuation allowed, no spaces)
- there is no symbolic ($) handling, e.g. `$100` is invalid

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
* top line comments can also use a star, to support org-modes
```
- comments are associated with the _preceeding_ element
  - _except_ if they are not indented, and then are associated with the _following_ element

### Declarations
- local items override global items (e.g. account commodity overrides journal commodity)
```
journal {name}                ;  [0..1] per file
  commodity {commodity}       ; (optional) accounting commodity
  cg {account tree}           ; (optional) default capital gains account
  note {text}                 ; (optional) free text
  convention {F7|F5}          ; (optional) accounting convention for first elements
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
  underlying {commodity}                  ; (optional) underlying of this derivative commodity
  class {commodity class}                 ; (optional) provides better treatment
  multiplier {factor}                     ; (optional) for leveraged instruments
  mtm                                     ; (optional) mark as mark-to-market treatment
  externalid {id} {value}                 ; (optional) [0..] id for linking things together
```

### Transaction elements
_Basic syntax_
- there can only be one missing (elided) amount
- for any non-directed (see below) transactions, entries will amounts will all match the elided line
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

 _Directed syntax_
 It is possible to override the default assignment mechanism to keep the journal shorter, or provide richer asset flow tracking
 ```
 {date} {payee} | {narrative}
   {account tree one}   {amount} ~{account tree two}
   {account tree three} {amount2}
   {account tree four}  {amount3}
   {account tree five}
```
In this example, `amount` is transferred between `one` to `two`, and `amount2` and `amount3` are transferred to `five`.

_Splits_
Splits follow a similar syntax, and the amounts should be the opposite sign to the original amount. The measure is not required because it must match the original measure.
 ```
 {date} {payee} | {narrative}
   {account tree one}   {amount}
    ~{account tree two} {amount2}
   {account tree three}
```
1. `amount2` (as part of `amount`) is directed to `two`
1. `amount1 + amount2` is directed to `three` (where `amount2` has the opposite sign already)

### Prices and [asset] splits
```
prices {commodity} {unit commodity}
  {date} {value}
  {date} {value}
  ...

{date} split {commodity} {old units} {new units}
```

### Assertions
```
{date} assert {account tree} {amount}
```
