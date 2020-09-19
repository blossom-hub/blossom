module Reports

open Types
open Shared

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
