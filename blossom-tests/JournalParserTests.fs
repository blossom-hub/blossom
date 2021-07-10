module JournalParserTests

open Xunit
open FParsec

open JournalParser
open ParserShared

let S0 = UserState.Default

let run1 p s = runParser (p .>> eof) S0 (FromString s)

[<Fact>]
let ``test`` () =
  Assert.True(true)

[<Fact>]
let ``can_parse_account`` () =
  let x = "Asset:Five"
  let actual = runParser pAccount S0 (FromString x)
  let expected = Types.Account "Asset:Five"
  Assert.Equal (expected, actual)

[<Fact>]
let ``can_parse_virtual_account`` () =
  let input = "Asset:Five/Virtual"
  let actual = run1 pAccount input
  let expected = Types.Account2 ("Asset:Five", "Virtual")
  Assert.Equal (expected, actual)