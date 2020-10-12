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
  // do pre-filter
  let j2 = prefilter request journal

  // run it
  let result = evaluateBalances j2 |> Map.toList
                                   |> List.tryLast
                                   |> Option.map (snd >> Map.toList >> List.map (second Map.toList))
                                   |> Option.defaultValue []

  // re-apply specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (Account a,_) -> regexfilter r a)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.map (second(List.filter (fun (Commodity c, _) -> regexfilter r c)))
                     |> List.filter (snd >> List.isEmpty >> not)

  let result = match request.flexmode with
                 | true -> result
                 | false -> result |> accountFilter |> commodityFilter

  // temporarily make a table
  let cs = [{Header = "Account"; Key = true}; {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}]
  let data = result |> List.collect (fun (Account a, vs) -> vs |> List.map (fun (Commodity m, q) -> [Text a; Number (q, 3); Text m]))

  let table = Table (cs, data)
  renderer table

let journal renderer request journal =
  // do pre-filter
  let j2 = prefilter request journal

  // expand entries
  let items =
    j2.Register |> Map.toList
                |> List.collect snd
                |> List.collect (fun e -> e.Postings |> List.collect (fun (a,b,c) -> expandPosting journal.CommodityDecls a b c)
                                                     |> List.map (fun (a,b,c) -> (e.Flagged, e.Date, e.Payee, e.Narrative, a, b, c)))

  // re-apply specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, _, _, Account a, _, _) -> regexfilter r a)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, _, _, _, _, Commodity c) -> regexfilter r c)

  let postfilter = match request.flexmode with
                     | true -> id
                     | false -> accountFilter >> commodityFilter

  let cs = [{Header = "Date"; Key=true}
            {Header = "F"; Key = true}
            {Header = "Payee"; Key = true}
            {Header = "Narrative"; Key =true}
            {Header = "Account"; Key = true}
            {Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let f2s = function true -> "*" | false -> ""
  let p2s = Option.defaultValue ""
  let createRow (f, d, p, n, Account a, q, Commodity c) = [Date d; f2s f |> Text; p2s p |> Text; Text n; Text a; Number(q, 3); Text c]
  let data = items |> postfilter |> List.map createRow
  let table = Table (cs, data)

  renderer table

let balanceSeries renderer tenor request journal =
   // define specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> Map.filter (fun (Account a) _ -> regexfilter r a)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> Map.map (fun _ m -> m |> Map.filter (fun (Commodity c) _ -> regexfilter r c))

  let postfilter = match request.flexmode with
                     | true -> id
                     | false -> Map.map (fun _ xs -> xs |> accountFilter |> commodityFilter)

  // run it -> filter it
  let result = prefilter request journal |> evaluateBalances
                                         |> postfilter

  // pick each balance date <= each schedule date
  let dates = result |> Map.toList |> List.map fst
  let left, right = dates |> (List.min &&& List.max)
  let schedule = makeSchedule tenor left right |> Set.toList

  // overall there aren't _too_ many elements to either date list
  let balances = schedule |> List.map (fun dt -> dates |> List.filter (fun d -> d <= dt) |> List.last |> fun d -> d, result.[d])

  let cs = [{Header = "Date"; Key=true}
            {Header = "Account"; Key = true}
            {Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let createRow (d, m) =
    m |> Map.toList
      |> List.collect (fun (Account a, bs) -> bs |> Map.toList
                                                 |> List.map (fun (Commodity m, q) -> [Date d; Text a; Number (q, 3); Text m]))

  let data = balances |> List.collect createRow
  let table = Table (cs, data)

  renderer table