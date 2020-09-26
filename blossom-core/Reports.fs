module Reports

open Types
open Shared
open Journal
open Tabular

let meta renderer request journal  =

  let accountsFromEntry entry =
    entry.Postings |> List.collect (fun (Account a, _, ca) -> match ca with Some (Account x) -> [a;x] | _ -> [a])

  let commoditiesFromAmount =
    function | V (_, Commodity c)             -> [c]
             | T ((_, Commodity c1), (_, Commodity c2)) -> [c1; c2]
             | X ((_, Commodity c1), (_, Commodity c2)) -> [c1; c2]

  let commoditiesFromEntry entry =
    entry.Postings |> List.collect (fun (_, a, _) -> commoditiesFromAmount a)

  renderer <|
    match request with
      | Accounts ->
          let entryAccounts = journal.Register |> Map.toList
                                               |> List.collect (snd >> List.collect accountsFromEntry)
          let declAccounts = journal.AccountDecls |> Map.toList
                                                  |> List.map (fun (Account a, _) -> a)
          let assertAccounts = journal.Assertions |> List.map (fun (_, Account a, _) -> a)
          [entryAccounts; declAccounts; assertAccounts] |> List.concat |> Set.ofList
      | Commodities ->
          let entryCommodities = journal.Register |> Map.toList
                                                  |> List.collect (snd >> List.collect commoditiesFromEntry)
          let declCommodities = journal.CommodityDecls |> Map.toList
                                                       |> List.map (fun (Commodity c, _) -> c)
          let priceCommodities = journal.Prices |> Map.toList
                                                |> List.collect (fun ((Commodity c1, Commodity c2), _) -> [c1; c2])
          let splitCommodities = journal.Splits |> Map.toList
                                                |> List.map (fun (Commodity c, _) -> c)
          [entryCommodities; declCommodities; priceCommodities; splitCommodities] |> List.concat |> Set.ofList
      | Payees -> journal.Register |> Map.toList |> List.collect (snd >> List.choose (fun p -> p.Payee)) |> Set.ofList


let balances renderer request journal =
  // do prefilter
  let j2 = prefilter request journal

  // run it
  let result = evaluateBalances j2 |> Map.toList
                                   |> List.tryLast
                                   |> Option.map (snd >> Map.toList >> List.map (second Map.toList))
                                   |> Option.defaultValue []

  // re-apply specific filters to crystalise the result
  // TODO make this an option
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (Account a,_) -> regexfilter r a)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.map (second(List.filter (fun (Commodity c, _) -> regexfilter r c)))
                     |> List.filter (snd >> List.isEmpty >> not)

  let result = result |> accountFilter |> commodityFilter

  // temporarily make a table
  let cs = [{Header = "Account"; Key = true}; {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}]
  let data = result |> List.collect (fun (Account a, vs) -> vs |> List.map (fun (Commodity m, q) -> [Text a; Number (q, 3); Text m]))

  let table = Table (cs, data)
  renderer table