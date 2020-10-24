module Reports

open System
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
                                   |> Option.map snd
                                   |> Option.defaultValue []

  // re-apply specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (Account a,_, _) -> regexfilter r a)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, Commodity c) -> regexfilter r c)

  let result = match request.flexmode with
                 | true -> result
                 | false -> result |> accountFilter |> commodityFilter

  // temporarily make a table
  let cs = [{Header = "Account"; Key = true}; {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}]
  let data = result |> List.map (fun (Account a, q, Commodity c) -> [Text a; Number (q, 3); Text c])

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

let balanceSeries renderer tenor cumulative request journal =
   // define specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (Account a, _, _) -> regexfilter r a)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, Commodity c) -> regexfilter r c)

  let postfilter = match request.flexmode with
                     | true -> id
                     | false -> List.map (second (accountFilter >> commodityFilter))

  // run it -> filter it
  let j2 = prefilter request journal
  let result = evaluateBalances j2 |> Map.toList |> postfilter

  let dates = result |> List.map fst
  let left, right = dates |> (List.min &&& List.max)
  let schedule = makeSchedule tenor left right |> Set.add left |> Set.toList

  let cs = [{Header = "Date"; Key=true}
            {Header = "Account"; Key = true}
            {Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  match left = right with
    | true -> renderer (Table(cs, []))
    | false ->
        let balances = schedule |> List.map (fun dt -> dt, result |> List.takeWhile (fun (d,_) -> d <= dt) |> List.last |> snd)
        match cumulative with
          | true -> let data= balances |> List.collect (fun (dt, xs) -> xs |> List.map (fun (Account a, q, Commodity c) -> [Date dt; Text a; Number (q, 3); Text c]))
                    Table (cs, data) |> renderer
          | false -> let balances2 = [(DateTime.MinValue, [])] @ balances
                                        |> List.pairwise
                                        |> List.map (fun ((_,xs), (dt,ys)) -> let xsn = xs |> List.map (fun (a, q ,c) -> (a, -q, c))
                                                                              let ds = summateAQCs (ys @ xsn)
                                                                              dt, ds)
                     let data = balances2 |> List.collect (fun (dt, xs) -> xs |> List.map (fun (Account a, q, Commodity c) -> [Date dt; Text a; Number (q, 3); Text c]))
                     Table (cs, data) |> renderer