// Testing the test framework to make sure things run okay in vscode
module InfraTests
open Xunit

[<Fact>]
let ``Always true 1`` () =
    Assert.True(true)

[<Theory>]
[<InlineData(1)>]
let ``Theory always true for 1`` (x:int) =
    Assert.Equal(1, x)