# Command Reference

All these commands operate on the `blossom-cli` main executable command internal prompt, e.g.

```
Welcome to Blossom v0.1
-----------------------
] :l my_file.dat
] :b >2020-01-01
```

Most commands have a on letter shortform (starting with a `:`) for those who like to type less, understand more.

> Many of these commands will translate to the interface inside the VSCode plugin later.

## Operational commands
### Load and reload files
```
] :l filename.dat
] load filename.dat

] :r
] reload
```

### Validate
Print the validation results
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
View the list of accounts / commodities etc.
```
] :meta accounts
] :meta commodities
```

## Reporting commands

> Example outputs will be later as the tool develops. A key decision is how to _actually_ filter - should the result be the full set of transactions, or just specific to the filter? e.g. if the transaction is an FX purchase `GBP<->USD`, and you filter by `%GBP`, should one see both legs? This can be somewhat confusing for balance reports so further testing required.

### Common filters
The following filters generally apply to most commands detailed below. `:c` is a stand-in for any command in the examples. Most textural entries accept regular expressions, and can be quoted for more complex expressions.

#### No filter symbol - filter accounts
No symbol means that the text is applied as a regular expression on the account name. An `!` at the beginning turns off the regular expression.

```
] :c Asset          ; all accounts with "Asset" in the text
] :c Wallet$        ; all accounts ending with "Wallet"
] :c !Asset         ; the account equal to "Asset"
```

#### Date ranges: `>` and `<`
The date is in the common format of `yyyy-MM-dd` _except_ that it the month and day elements are optional. If they are omitted, then the January or 1 day are used. For `<`, it will substituted to the day before.

Dates are inclusive.

```
] :c >2020                ; [2020-01-01, max date in journal]
] :c <2021                ; [min date in journal, 2020-12-31]
] :c <2021-01-03          ; [min date in journal, 2020-01-03]
] :c >2020-03 <2020-04    ; [2020-03-01, 2020-01-31]
] :c >2020-04-04 <2021-10 ; [2020-04-04, 2020-09-30]
```

#### Payee filter: `@`
A regular expression against the payee field in transactions.
```
] :c @Google      ; Payee contains "Google"
```

#### Narrative: `?`
Similar to payee filter, except on the narrative field.
```
] :c "Work lunch"
```

#### Commodity: `%`
Getting results with the specific commodity:
```
] :c %JPY     ; query by those including Japanese Yen
```

### Expanded examples
```
] :c Income >2020 <2021 %JPY    ; All income accounts with transactions in 2020
] :s "Groceries|Eating out" %USD >2020 <2020-06
```

### Balance query - `:b` / `bal` / `balances`

### Balance series query - `:s` / `series`

### View journal - `:j` / `journal`


