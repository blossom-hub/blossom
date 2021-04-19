module JournalTests

open System

open Journal
open JournalParser
open Types

open Xunit

let testCommodity = Commodity "Alpha"
let testDate1 = DateTime(2021,1,1)

let account1 = Account "Account1"
let account2 = Account "Account2"
let account3 = Account "Account3"

let runBalancerChecker f g h elts =
  let dtransfer = {Payee = Some "payee"; Narrative = "test 1"; Tags = Set.empty; Entries = elts}
  let result = liftBasicEntry "In unit test" (testDate1, None) false dtransfer
  match result with
    | Choice2Of2 msg   -> match g with Some gg -> gg msg | None -> failwith $"Unexpected error: {msg}"
    | Choice1Of2 entry -> h entry

[<Fact>]
let ``Balance Check 1`` () =
  let elts = [
    Posting(account1, Some (3.4M, testCommodity), Some (CV account2))
  ]
  let f entry =
    let ps = entry.Postings
    Assert.True(List.length ps = 1)
  runBalancerChecker None None f elts

[<Fact>]
let ``Balance Check 2`` () =
  let elts = [
    Posting(account1, Some (3.4M, testCommodity), None)
    Posting(account2, None, None)
  ]
  let f entry =
    let ps = entry.Postings
    Assert.True(List.length ps = 1)
  runBalancerChecker None None f elts

[<Fact>]
let ``Balance Check 3`` () =
  let elts = [
    Posting(account1, Some (3.4M, testCommodity), None)
    Posting(account2, Some (3.4M, Commodity "USD"), None)
    Posting(account3,None, None)
  ]
  let f entry =
    let ps = entry.Postings
    Assert.True(List.length ps = 2)
  runBalancerChecker None None f elts