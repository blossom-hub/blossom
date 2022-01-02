# Command Reference

All these commands operate on the `blossom-cli` main executable command internal prompt, e.g.

```
Welcome to Blossom v0.1
-----------------------
] :l my_file.dat
] :b >2020-01-01
```

Most commands have a on letter shortform (starting with a `:`) for those who like to type less, understand more. Some take optional flags, which are provided using the syntax
```
] :c -abc <other args>
```
where a,b,c and are all flags.

> Many of these commands will translate to the interface inside the VSCode plugin later.

## Operational commands
### Load and reload files
```
] :l filename.dat
] load filename.dat

] :r
] reload
```

### Help
A short help message is printed to remind some syntax.
```
] help
  Filters
    date: >,>=,<, or <=
    payee: @
    narrative: ?
    commodity: %
    measure: %%
    tag: +
    virtual account: /
    account: no symbol
```

### Validation
Print the validation results (not yet implemented).
```
] :v
] validate
```

### Clear screen
```
] cls
```

### Quit
```
] :q
] quit
```

### Meta
View the list of accounts / commodities etc. Any text after the type of meta is likely to be used as a regex filter for that dataset.
```
] meta accounts
] meta commodities
] meta payees
] meta <others>
```

## Common filters
The following filters generally apply to most commands detailed below. `:c` is a stand-in for any command in the examples. Most textural entries accept regular expressions, and can be quoted for more complex expressions.

Filters run in two modes (where supported) with respect to commodities/postings: `strict` (default) will filter everything down to give a precise result, whereas `flex` mode will keep related postings if the entry passes initial scanning. Consider the following entry:
```
2020-01-01 Buy some flowers
  Assets:Wallet     -120 USD
  Expenses:Flowers
```
And now consider these two outputs:
```
] :b Assets
      Account   Balance   Commodity
Assets:Wallet      -120         USD
] :b -x Assets
         Account   Balance   Commodity
   Assets:Wallet      -120         USD
Expenses:Flowers       120         USD
```
In the second output, the whole of the entry is considered for reporting, whereas in the first, only the `Assets:Wallet` will be reported.

This flag is controlled (as seen above) using the `-x` flag on the query.

### Common flags
Some commands support other flags, which must be specified near the start of the command.

`-g` collapses accounts names to the top value; currently supported by balances and series.

`-x` as per above on most commands.

`-z` hide some zero rows if applicable.

`-v` includes any virtual account in the account string.

If an invalid flag is provided, an error will be provided.

### No filter symbol - filter accounts
No symbol means that the text is applied as a regular expression on the account name.

```
] :c Asset          ; all accounts with "Asset" in the text
] :c Wallet$        ; all accounts ending with "Wallet"
```

### Date ranges: `>`, `<`, `>=`, and `<=`
The date is in the common format of `yyyy-MM-dd` _except_ that it the month and day elements are optional. If they are omitted, then the January or 1 day are used.

```
] :c >2020                 ; [2020-01-02, max date in journal]
] :c <2021                 ; [min date in journal, 2020-12-31]
] :c <2021-01-03           ; [min date in journal, 2020-01-02]
] :c <=2021-01-03          ; [min date in journal, 2020-01-03]
] :c >2020-03 <2020-04     ; [2020-03-02, 2020-03-31]
] :c >2020-03 <=2020-04    ; [2020-03-02, 2020-04-01]
] :c >2020-04-04 <2021-10  ; [2020-04-05, 2020-09-30]
```

### Payee filter: `@`
A regular expression against the payee field in transactions.
```
] :c @Google      ; Payee contains "Google"
```

### Narrative: `?`
Similar to payee filter, except on the narrative field.
```
] :c ?"Work lunch"
```

### Commodity: `%`
Getting results with the specific commodity:
```
] :c %JPY     ; query by those including Japanese Yen
```

### Measure: `%%`
Getting results with the specific commodity as a measure. For example, stocks in Japanese Yen. Cash items are measured in their own commodity.
> The measure commodity is determined by the first trading entry for a commodity, or otherwise, defaults to itself.
```
] :c %%JPY     ; query all assets meausured (denominated) in Japanese Yen
```

## Accounting commands

### Balance query - `:b` / `bal` / `balances`
```
] :b [-gzxv] <filter string>
             Account  Balance  Commodity  Name
     Asset:Bank:Cash      100        USD
Asset:Broker:Trading        5       MSFT  Microsoft Corp.
```
Balances now contain the commodity name, if provided in a `commodity` declaration.


### Balance series query - `:s` / `series`
```
] :s <tenor> [-gzxvc] <filter string>
```
Group balances by the tenor at the account level. The default output is on a periodic basis, add the `c` flag to run on a cumulative basis instead.
A tenor is defined as Y/H/Q/M (W is not yet supported). The final date is used for each period, capped/floored if there are insufficent dates.

```
] :s Q+
=> BalanceSeries (Q+, "")
      Date                     Account   Amount   Commodity
2020-01-06            Assets:Account 1        2       ASSET
                                            145         EUR
                                             10         GBP
                                              2       NQZ20
                                         -45065         USD
2020-01-06            Assets:Account 2     -145         EUR
                                           -100         GBP
                                           -104         USD
2020-01-06     Assets:Settlement Alpha     -100         USD
```

### View journal - `:j` / `journal`
```
] :j [-zfxv] <filter string>
```
Displays the register from the journal.
```
=> Journal ""
      Date   F   Payee           Narrative                     Account   Amount   Commodity
2020-01-01          Me    Basic Transfer 1            Assets:Account 1      100         USD
2020-01-01          Me    Basic Transfer 1     Assets:Settlement Alpha     -100         USD
2020-01-02   *      SO    Basic Transfer 2            Assets:Account 1      100         USD
                                                                           -100
```

## Investment commands
These commands operate slightly differently to the reporting commands, but typically keep the same filters if they make sense.

### Lots summary - `lots`
Displays a lot report, showing opening / closing pairs, PnL etc. This is a new report and under development. See [Trading with Blossom](trading.md).
```
] lots [-xog]
```

## "Checks and balances" commands
```
] :c <type of check>
```
Currently supported checks are: `assertions`

#### (Balance) Assertion checks
This will check the balance (in the relevant commodity) is correct verus the assertion in the journal, and report the delta.
```
] :c assertions
=> Check Assertions
      Date            Account   Commodity   Expected   Actual   Delta
2020-02-01   Assets:Account 2         USD       -106     -104       2
```
