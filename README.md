# blossom
Double entry plain text accounting for traders

Blossom is a _yet another_ plain text acount application cli similar to [Ledger-cli](https://github.com/ledger/), [Hledger](https://github.com/simonmichael/hledger) and [Beancount](https://github.com/beancount). As with the other similar implementations:
- blossom works locally, without interacting with webs servers, banks or the like, your data stays with you.
- As it's plain text, you can store your files in whatever source control repository or document management system you like. I **don't** recommend storing them on github.com!
- blossom only reads your data, it doesn't know how to write and won't smash up your data.
- a small eco-system of helper utilities is in the planning (pretty-print, price import, etc - rather custom)

## Does it work yet?
No, this repository and build is in early days.

## Why is there _another_ clone of ledger?
1. I find that the other systems don't cater well for more advanced trading strategies such as options or futures, which large numbers of trading assets (100+ can soon accrue).
1. My use of MS Money for the last 15 years didn't really cut it for trading and multicurrency handling; it's long dead and I needed another solution.
1. I want to customize reports inside the app rather than have to write on top of results from others.
1. I felt like a challenge!

## Differences to others
After the initial accounting portions which are fairly standard across pta software (balance checking, validations, reporting etc), there is a focus on trading support:
- Enhanced PnL reporting taking into account expenses, transfers, cross currency impact
- Support for non-nav assets such as mtm futures
- Possible future support for risk, stress reports (option evaluation, derivatives linkage to underlyings, volatility surfaces etc) for OTC products.

Most of the "standard" formatting works in blossom, although there some extra helpers and formatting supported to cut down on boilerplate and monotonous copy/paste.

## Plans
I'm currently migrating a bigger codebase from my initial implementation into this repository and upgrading several features.
1. Migrate existing infrastructure
1. Code up and improve original accouting
1. Add more checks / validation
1. Focus trading expanding features
1. Add a [VSCode](https://code.visualstudio.com/) extension for both _editing_ and _processing_ the data.