module Reports

open System
open Types
open Shared
open Journal
open Tabular

// utilities
let groupTopn iv n =
  List.map (first3 (splitAccounts >> function (AccountHierarchy (xs, _)) -> xs.[0..n-1] |> fun x -> AccountHierarchy (x, None) |> joinAccounts)) >> summateAQCs iv

let meta renderer request journal =
  let journalList = journal.Register |> Map.toList

  let accountsFromEntry entry =
    let f acc = let a = getAccount acc
                let b = getVirtualAccount acc
                match b with Some c -> [a; $"{a}/{c}"] | None -> [a]
    entry.Postings |> List.collect (fun (acc, _, ca) -> f acc @ (Option.fold (fun _ c -> f c) [] ca))

  let commoditiesFromAmount =
    function | V (_, Commodity c)             -> [c]
             | T ((_, Commodity c1), (_, Commodity c2)) -> [c1; c2]
             | X ((_, Commodity c1), (_, Commodity c2)) -> [c1; c2]

  let commoditiesFromEntry entry =
    entry.Postings |> List.collect (fun (_, a, _) -> commoditiesFromAmount a)

  let accounts =
    let entryAccounts = journalList |> List.collect (snd >> List.collect accountsFromEntry)
    let declAccounts = journal.AccountDecls |> Map.toList
                                            |> List.map (fun (a, _) -> getAccount a)
    let assertAccounts = journal.Assertions |> List.map (fun (_, a, _) -> getAccount a)
    [entryAccounts; declAccounts; assertAccounts] |> List.concat |> Set.ofList

  let commodities =
    let entryCommodities = journalList |> List.collect (snd >> List.collect commoditiesFromEntry)
    let declCommodities = journal.CommodityDecls |> Map.toList
                                                 |> List.map (fun (Commodity c, _) -> c)
    let priceCommodities = journal.Prices |> Map.toList
                                          |> List.collect (fun ((Commodity c1, Commodity c2), _) -> [c1; c2])
    let splitCommodities = journal.Splits |> Map.toList
                                          |> List.map (fun (Commodity c, _) -> c)
    [entryCommodities; declCommodities; priceCommodities; splitCommodities] |> List.concat |> Set.ofList

  let payees = journalList |> List.collect (snd >> List.choose (fun p -> p.Payee)) |> Set.ofList

  let hashtags=
    journalList |> List.collect (fun (_, es) -> es |> List.map (fun p -> p.HashTags))
                |> Set.unionMany

  let transactionCount = journalList |> List.collect snd |> List.length
  let entryCount = journalList |> List.collect snd |> List.collect (fun e -> e.Postings) |> List.length

  renderer <|
    match request with
      | MetaRequest.Statistics -> Statistics {
            Range = journalList |> List.map fst |> (List.min &&& List.max)
            Transactions = transactionCount, entryCount
            Accounts = accounts |> Set.count
            Commodities = commodities |> Set.count
            Payees = payees |> Set.count
            Hashtags = hashtags |> Set.count
            Assertions = journal.Assertions |> List.length
            Prices = journal.Prices |> Map.toList |> List.sumBy (snd >> Map.count)
          }
      | Accounts -> accounts |> MetaResultSet
      | Commodities -> commodities |> MetaResultSet
      | Payees -> payees |> MetaResultSet
      | HashTags -> hashtags |> MetaResultSet

let checkJournal renderer request journal =
  let checkAssertion asofBalances dt account quantity commodity =
    let bals = asofBalances |> Map.tryFind dt
    let actual = bals |> Option.bind (Map.tryFind (account, commodity)) |> Option.defaultValue 0M
    match actual = quantity with
      | false -> Some actual
      | true  -> None

  renderer <|
    match request with
      | Assertions -> let balances = evaluateBalances journal |> Map.toList
                      let assertions = journal.Assertions
                      let schedule = assertions |> List.map fst3
                      let f1 = (fun dt -> dt, balances |> List.takeWhile (fun (d,_) -> d <= dt) |> List.last |> snd)
                      let f2 = List.groupByApply (fst3 &&& thd3) (List.sumBy snd3) >> Map.ofList
                      let asofBalances = schedule |> List.map (f1 >> second f2) |> Map.ofList
                      let check1 (dt, a, (q, c)) =
                        let r = checkAssertion asofBalances dt a q c
                        match r with
                          | Some actual -> Some (dt, a, q, c, actual)
                          | None        -> None
                      let fails = assertions |> List.choose check1
                                             |> List.sortBy (fun (dt, a, _, c, _) -> (dt,a,c))
                                             |> List.map (fun (dt, a, q, Commodity c, actual) -> [Date dt; getAccount a |> Text; Text c; Number (q, 3); Number (actual, 3); Number (actual-q, 3)])
                      let cs = [{Header = "Date"; Key=true}
                                {Header = "Account"; Key = true}
                                {Header = "Commodity"; Key = true}
                                {Header = "Expected"; Key = false}
                                {Header = "Actual"; Key = false}
                                {Header = "Delta"; Key = false}]
                      Table (cs, fails)

let balances renderer request flags journal =
  // do pre-filter
  let j2 = prefilter request journal

  // run it
  let includeVirtual, flags = Set.pop "v" flags
  let result =
    evaluateBalances j2
      |> Map.toList
      |> List.tryLast
      |> Option.map snd
      |> Option.defaultValue []

  // re-apply specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (a, _, _) -> getAccount a |> regexfilter r)

  let subAccountFilter xs =
    match request.subaccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun (a,_, _) -> match getVirtualAccount a with | Some t -> regexfilter r t | _ -> false)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, Commodity c) -> regexfilter r c)

  let zeroFilter = List.filter (fun (_, q, _) -> q <> 0M)

  // react to flags
  let isFlex, flags = Set.pop "flex" flags
  let isGpTop, flags = Set.pop "t" flags
  let hideZero, flags = Set.pop "z" flags

  let result2 = result |> iftrue (not isFlex) (accountFilter >> subAccountFilter >> commodityFilter)
                       |> iftrue isGpTop (groupTopn includeVirtual 1)
                       |> iftrue hideZero zeroFilter

  // temporarily make a table
  let table =
    match includeVirtual with
      | true  -> let c = [{Header = "Account"; Key = true};  {Header = "V"; Key = true};
                          {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}]
                 let d = result2 |> List.map (fun (a, q, Commodity c) -> (a, q, c))
                                 |> List.sortBy fst3
                                 |> List.map (fun (a, q, c) -> let s2 = getVirtualAccount a |> Option.defaultValue ""
                                                               [getAccount a |> Text; Text s2; Number (q, 3); Text c])
                 Table (c, d)
      | false -> let c = [{Header = "Account"; Key = true}; {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}]
                 let d = result2 |> List.map (fun (a, q, c) -> (a, q, c))
                                 |> summateAQCs false
                                 |> List.sortBy fst3
                                 |> List.map (fun (a, q, Commodity c) -> [getAccount a |> Text; Number (q, 3); Text c])
                 Table (c, d)

  renderer table

let journal renderer request flags journal =
  let includeVirtual, flags = Set.pop "v" flags

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
      | Some r -> xs |> List.filter (fun (_, _, _, _, a, _, _) -> getAccount a |> regexfilter r)

  let subAccountFilter xs =
    match request.subaccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, _, _, a, _, _)-> match getVirtualAccount a with Some t -> regexfilter r t | _ -> false)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, _, _, _, _, Commodity c) -> regexfilter r c)

  let flaggedFilter xs =
    xs |> List.filter (fun (x, _, _, _, _, _, _) -> x)

  // react to flags
  let isFlex, flags = Set.pop "flex" flags
  let onlyFlagged, flags = Set.pop "f" flags

  let result2 = items |> iftrue (not isFlex) (accountFilter >> subAccountFilter >> commodityFilter)
                      |> iftrue onlyFlagged flaggedFilter

  let cs = [{Header = "Date"; Key=true}
            {Header = "F"; Key = true}
            {Header = "Payee"; Key = true}
            {Header = "Narrative"; Key =true}
            {Header = "Account"; Key = true}]
           @ (match includeVirtual with true -> [{Header = "VA"; Key = true}] | false -> []) @
           [{Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let f2s = function true -> "*" | false -> ""
  let p2s = Option.defaultValue ""

  let removeSubAccount xs =
    match includeVirtual with
      | true -> xs
      | false -> xs |> List.groupByApply (fun (f, d, p, n, a, _, c) -> (f, d, p, n, Account (getAccount a), c))
                                         (List.sumBy (fun (_, _, _, _, _, q, _) -> q))
                    |> List.map (fun ((f, d, p, n, a, c), q) -> (f, d, p, n, Account (getAccount a), q ,c))

  let createRow (f, d, p, n, a, q, Commodity c) =
    match includeVirtual with
      | true  -> [Date d; f2s f |> Text; p2s p |> Text; Text n; getAccount a |> Text; getVirtualAccount a |> p2s |> Text; Number(q, 3); Text c]
      | false -> [Date d; f2s f |> Text; p2s p |> Text; Text n; getAccount a |> Text; Number(q, 3); Text c]

  let data = result2 |> removeSubAccount |> List.map createRow
  let table = Table (cs, data)

  renderer table

let balanceSeries renderer tenor cumulative request flags journal =
  let includeVirtual, flags = Set.pop "v" flags

  // define specific filters to crystalise the result
  let accountFilter xs =
    match request.account with
      | None -> xs
      | Some r -> xs |> List.filter (fun (a, _, _) -> getAccount a |> regexfilter r)

  let subAccountFilter xs =
    match request.subaccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun (a,_, _) -> getVirtualAccount a |> function | Some t -> regexfilter r t | _ -> false)

  let commodityFilter xs =
    match request.commodity with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, Commodity c) -> regexfilter r c)

  let zeroFilter = List.filter (fun (_, q, _) -> q <> 0M)

  // run it -> filter it
  let j2 = prefilter request journal
  let result = evaluateBalances j2 |> Map.toList

  // react to flags
  let isFlex, flags = Set.pop "flex" flags
  let isGpTop, flags = Set.pop "t" flags

  let result = result |> iftrue (not isFlex) (List.map (second (accountFilter >> subAccountFilter >> commodityFilter)))
                      |> iftrue isGpTop (List.map (second (groupTopn includeVirtual 1)))

  let dates = result |> List.map fst
  let left, right = dates |> (List.min &&& List.max)
  let schedule = makeSchedule tenor left right |> Set.add left |> Set.toList

  let cs = [{Header = "Date"; Key=true}
            {Header = "Account"; Key = true}]
           @ (match includeVirtual with true -> [{Header = "V"; Key = true}] | false -> []) @
           [{Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let p2s = Option.defaultValue ""
  let createRow dt (a, q, Commodity c) =
    match includeVirtual with
      | true  -> [Date dt; getAccount a |> Text; getVirtualAccount a |> p2s |> Text; Number (q, 3); Text c]
      | false -> [Date dt; getAccount a |> Text; Number (q, 3); Text c]

  match left = right with
    | true -> renderer (Table(cs, []))
    | false ->
        let balances0 = schedule |> List.map (fun dt -> dt, result |> List.takeWhile (fun (d,_) -> d <= dt) |> List.last |> snd)
        let balances =
          match includeVirtual with
            | true  -> balances0
            | false -> balances0 |> List.map (second (List.groupByApply (fun (a, _, c) -> (Account (getAccount a), c))
                                                                        (List.sumBy (fun (_, q, _) -> q))
                                                        >> List.map (fun ((a, c), q) -> (a,q,c))))
        match cumulative with
          | true -> let data= balances |> List.collect (fun (dt, xs) -> xs |> List.map (createRow dt))
                    Table (cs, data) |> renderer
          | false -> let balances2 = [(DateTime.MinValue, [])] @ balances
                                        |> List.pairwise
                                        |> List.map (fun ((_,xs), (dt,ys)) -> let xsn = xs |> List.map (fun (a, q ,c) -> (a, -q, c))
                                                                              let ds = summateAQCs includeVirtual (ys @ xsn)
                                                                              dt, ds)
                     let data = balances2 |> List.collect (fun (dt, xs) -> xs |> List.map (createRow dt))
                     Table (cs, data) |> renderer