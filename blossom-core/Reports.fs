module Reports

open System
open Analysis
open Types
open Shared
open Journal
open Tabular

// utilities
let groupTopn iv n =
  let f = splitAccounts >> fst >> List.take n >> (fun x -> joinAccounts x None)
  List.map (first3 f) >> summateAQCs iv

let meta renderer request journal =
  let journalList = journal.Register |> Map.toList

  let accountsFromEntry entry =
    let f acc = let a = getAccount acc
                let b = getVirtualAccount acc
                match b with Some c -> [a; $"{a}/{c}"] | None -> [a]
    entry.Postings |> List.collect (fun (acc, _, ca) -> f acc @ f ca) |> List.distinct

  let commoditiesFromEntry entry = entry.Postings |> List.map (fun (_, (_, Commodity c), _) -> c)

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

  let tags =
    journalList |> List.collect (fun (_, es) -> es |> List.map (fun p -> p.Tags))
                |> Set.unionMany

  let transactionCount = journalList |> List.collect snd |> List.length
  let entryCount = journalList |> List.collect snd |> List.collect (fun e -> e.Postings) |> List.length

  let fn = match request.Regex with None -> id | Some p -> Set.filter (fun r -> regexfilter p r)

  renderer <|
    match request.RequestType with
      | MetaRequestType.Statistics ->
          Statistics {
            Range = journalList |> List.map fst |> ((List.min >> fst) &&& (List.max >> fst))
            Transactions = transactionCount, entryCount
            Accounts = accounts |> Set.count
            Commodities = commodities |> Set.count
            Payees = payees |> Set.count
            Tags = tags |> Set.count
            Assertions = journal.Assertions |> List.length
            Prices = journal.Prices |> Map.toList |> List.sumBy (snd >> Map.count)
          }
      | Accounts    -> fn accounts |> MetaResultSet
      | Commodities -> fn commodities |> MetaResultSet
      | Payees      -> fn payees |> MetaResultSet
      | Tags_       -> fn tags |> MetaResultSet

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
                      let f1 = (fun dt -> dt, balances |> List.takeWhile (fun (d,_) -> fst d <= dt) |> List.last |> snd)
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

let balances renderer filter (request : BalancesRequest) journal =
  // do pre-filter
  let j2 = prefilter filter journal

  // run it
  let result =
    evaluateBalances j2
      |> Map.toList
      |> List.tryLast
      |> Option.map snd
      |> Option.defaultValue []

  // re-apply specific filters to crystalise the result
  let accountFilter xs =
    match filter.Accounts with
      | [] -> xs
      | rs -> xs |> List.filter (fun (acc, _, _) ->
                                   rs |> List.map (fun r -> getAccount acc |> regexfilter r)
                                      |> List.any id)

  let virtualAccountFilter xs =
    match filter.VAccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun (a,_, _) -> match getVirtualAccount a with | Some t -> regexfilter r t | _ -> false)

  let commodityFilter xs =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, Commodity c) ->
                                  rs |> List.map (fun r -> regexfilter r c)
                                     |> List.any id)

  let zeroFilter = List.filter (fun (_, q, _) -> q <> 0M)

  let result2 = result |> iftrue (not request.Flex) (accountFilter >> virtualAccountFilter >> commodityFilter)
                       |> iftrue request.GroupToTop (groupTopn request.ShowVirtual 1)
                       |> iftrue request.HideZeros zeroFilter

  // temporarily make a table
  let gcn c = journal.CommodityDecls |> Map.tryFind c |> Option.bind (fun t -> t.Name) |> Option.defaultValue ""
  let table =
    match request.ShowVirtual with
      | true  -> let c = [{Header = "Account"; Key = true};  {Header = "V"; Key = true};
                          {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}; {Header ="Name"; Key = false}]
                 let d = result2 |> List.map (fun (a, q, Commodity c) -> (a, q, c, gcn (Commodity c)))
                                 |> List.sortBy fst4
                                 |> List.map (fun (a, q, c, cn) -> let s2 = getVirtualAccount a |> Option.defaultValue ""
                                                                   [getAccount a |> Text; Text s2; Number (q, 3); Text c; Text cn])
                 Table (c, d)
      | false -> let c = [{Header = "Account"; Key = true}; {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}; {Header ="Name"; Key = false}]
                 let d = result2 |> List.map (fun (a, q, c) -> (a, q, c))
                                 |> summateAQCs false
                                 |> List.sortBy fst3
                                 |> List.map (fun (a, q, Commodity c) -> [getAccount a |> Text; Number (q, 3); Text c; Text (gcn (Commodity c))])
                 Table (c, d)

  renderer table

let journal renderer filter (request : JournalRequest) journal =
  // do pre-filter
  let j2 = prefilter filter journal

  // expand entries
  let items =
    j2.Register |> Map.toList
                |> List.collect snd
                |> List.collect (fun e -> e.Postings |> List.collect (fun (a,b,c) -> expandPosting a b c)
                                                     |> List.map (fun (a,b,c) -> (e.Flagged, e.Date, e.Payee, e.Narrative, a, b, c)))

  // re-apply specific filters to crystalise the result
  let accountFilter xs =
    match filter.Accounts with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, _, _, a, _, _) ->
                                   rs |> List.map (fun r -> getAccount a |> regexfilter r)
                                      |> List.any id)

  let virtualAccountFilter xs =
    match filter.VAccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun (_, _, _, _, a, _, _)-> match getVirtualAccount a with Some t -> regexfilter r t | _ -> false)

  let commodityFilter xs =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, _, _, _, _, Commodity c) ->
                                   rs |> List.map (fun r -> regexfilter r c)
                                      |> List.any id)

  let flaggedFilter xs =
    xs |> List.filter (fun (x, _, _, _, _, _, _) -> x)

  let result2 = items |> iftrue (not request.Flex) (accountFilter >> virtualAccountFilter >> commodityFilter)
                      |> iftrue request.FlaggedOnly flaggedFilter

  let cs = [{Header = "Date"; Key=true}
            {Header = "F"; Key = true}
            {Header = "Payee"; Key = true}
            {Header = "Narrative"; Key =true}
            {Header = "Account"; Key = true}]
           @ (match request.ShowVirtual with true -> [{Header = "VA"; Key = true}] | false -> []) @
           [{Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let f2s = function true -> "*" | false -> ""
  let p2s = Option.defaultValue ""

  let removeVirtualAccount xs =
    match request.ShowVirtual with
      | true -> xs
      | false -> xs |> List.groupByApply (fun (f, d, p, n, a, _, c) -> (f, d, p, n, stripVirtualAccount a, c))
                                         (List.sumBy (fun (_, _, _, _, _, q, _) -> q))
                    |> List.map (fun ((f, d, p, n, a, c), q) -> (f, d, p, n, stripVirtualAccount a, q ,c))

  let createRow (f, d, p, n, a, q, Commodity c) =
    match request.ShowVirtual with
      | true  -> [Date (fst d); f2s f |> Text; p2s p |> Text; Text n; getAccount a |> Text; getVirtualAccount a |> p2s |> Text; Number(q, 3); Text c]
      | false -> [Date (fst d); f2s f |> Text; p2s p |> Text; Text n; getAccount a |> Text; Number(q, 3); Text c]

  let data = result2 |> removeVirtualAccount |> List.map createRow
  let table = Table (cs, data)

  renderer table

let balanceSeries renderer filter (request : SeriesRequest) journal =
  // do pre-filter
  let j2 = prefilter filter journal

  // define specific filters to crystalise the result
  let accountFilter xs =
    match filter.Accounts with
      | [] -> xs
      | rs -> xs |> List.filter (fun (a, _, _) ->
                                   rs |> List.map (fun r -> getAccount a |> regexfilter r)
                                      |> List.any id)

  let virtualAccountFilter xs =
    match filter.VAccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun (a,_, _) -> getVirtualAccount a |> function | Some t -> regexfilter r t | _ -> false)

  let commodityFilter xs =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, Commodity c) ->
                                   rs |> List.map (fun r -> regexfilter r c)
                                      |> List.any id)

  let zeroFilter = List.filter (fun (_, q, _) -> q <> 0M)

  let result = evaluateBalances j2 |> Map.toList
                  |> iftrue (not request.Flex) (List.map (second (accountFilter >> virtualAccountFilter >> commodityFilter)))
                  |> iftrue request.GroupToTop (List.map (second (groupTopn request.ShowVirtual 1)))

  let dates = result |> List.map (fst >> fst)
  let left, right = dates |> (List.min &&& List.max)
  let schedule = makeSchedule request.Tenor left right |> Set.add left |> Set.toList

  let cs = [{Header = "Date"; Key=true}
            {Header = "Account"; Key = true}]
           @ (match request.ShowVirtual with true -> [{Header = "V"; Key = true}] | false -> []) @
           [{Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let p2s = Option.defaultValue ""
  let createRow dt (a, q, Commodity c) =
    match request.ShowVirtual with
      | true  -> [Date dt; getAccount a |> Text; getVirtualAccount a |> p2s |> Text; Number (q, 3); Text c]
      | false -> [Date dt; getAccount a |> Text; Number (q, 3); Text c]

  match left = right with
    | true -> renderer (Table(cs, []))
    | false ->
        let balances0 = schedule |> List.map (fun dt -> dt, result |> List.takeWhile (fun (d,_) -> fst d <= dt) |> List.last |> snd)
        let balances =
          match request.ShowVirtual with
            | true  -> balances0
            | false -> balances0 |> List.map (second (List.groupByApply (fun (a, _, c) -> (stripVirtualAccount a, c))
                                                                        (List.sumBy (fun (_, q, _) -> q))
                                                        >> List.map (fun ((a, c), q) -> (a,q,c))))
        match request.Cumulative with
          | true -> let data= balances |> List.collect (fun (dt, xs) -> xs |> List.map (createRow dt))
                    Table (cs, data) |> renderer
          | false -> let balances2 = [(DateTime.MinValue, [])] @ balances
                                        |> List.pairwise
                                        |> List.map (fun ((_,xs), (dt,ys)) -> let xsn = xs |> List.map (fun (a, q ,c) -> (a, -q, c))
                                                                              let ds = summateAQCs request.ShowVirtual (ys @ xsn)
                                                                              dt, ds)
                     let data = balances2 |> List.collect (fun (dt, xs) -> xs |> List.map (createRow dt))
                     Table (cs, data) |> renderer


type private InternalLotRow = {
  OpeningDate: SQ
  Account: Account
  Closed: bool
  Asset: Commodity
  Quantity: Decimal
  Long: bool
  OpeningPrice: Value
  ClosingDate: SQ option
  ClosingPrice: Value option
  Pnl: Value option
  ReturnPct: decimal option
}

let lotAnalysis renderer (filter: Filter) (request: LotRequest) journal =
  let quoteDPFor commodity = journal.CommodityDecls |> Map.tryFind commodity
                                                    |> Option.bind (fun t -> t.QuoteDP)
                                                    |> Option.defaultValue 3
  let nameFor commodity = journal.CommodityDecls |> Map.tryFind commodity
                                                 |> Option.bind (fun t -> t.Name)
                                                 |> Option.defaultValue ""

  let accountFilter (xs: OpeningTrade list) =
    match filter.Accounts with
      | [] -> xs
      | rs -> xs |> List.filter (fun ot -> let a = ot.Account |> getAccount
                                           rs |> List.map (fun r -> regexfilter r a) |> List.any id)

  let virtualAccountFilter (xs: OpeningTrade list) =
    match filter.VAccount with
      | None -> xs
      | Some r -> xs |> List.filter (fun ot -> let a = ot.Account |> getVirtualAccount
                                               match a with
                                                 | None -> false
                                                 | Some v -> regexfilter r v)

  let commodityFilter (xs: OpeningTrade list) =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun ot -> let (Commodity c) = ot.Asset
                                           rs |> List.map (fun r -> regexfilter r c) |> List.any id)

  // analysis has already completed, so most work here is the filter application
  let trades =
    journal.InvestmentAnalysis
      |> (accountFilter >> virtualAccountFilter >> commodityFilter)

  // create an anon-record representing the row data before
  // transforming to the concrete type, to make it easier to track
  // calcs and types
  let createRow (ot: OpeningTrade) =
    let row (ct: ClosingTrade option) =
      let rowQty = ct |> Option.map (fun c -> -c.Quantity) |> Option.defaultValue ot.Quantity
      let retn = ct |> Option.map (fun c -> let cp = fst c.PerUnitPrice
                                            let op = fst ot.PerUnitPrice
                                            let sg = if rowQty > 0M then 1M else -1M
                                            100M * sg * (cp - op) / op)
      {
        OpeningDate = ot.Date
        Account = ot.Account
        Closed = match ct with Some _ -> true | _ -> false
        Asset = ot.Asset
        Quantity = rowQty
        Long = rowQty > 0M
        OpeningPrice = ot.PerUnitPrice
        ClosingDate = ct |> Option.map (fun c -> c.Date)
        ClosingPrice = ct |> Option.map (fun c -> c.PerUnitPrice)
        Pnl = ct |> Option.map (fun c -> c.UnadjustedPnL)
        ReturnPct = retn
      }
    match ot.Closings with
      | [] -> [row None]
      | xs -> xs |> List.map (fun x -> row (Some x))

  let consolidate rows =
    let grouped = rows |> List.groupBy (fun r -> (r.OpeningDate, r.Long, r.Account, r.Closed, r.Asset, snd r.OpeningPrice))
    let aggr ((date, long, acct, closed, asset, measure), rs) =
      let quantity = rs |> List.sumBy (fun r -> r.Quantity)
      let openingPrice = rs |> List.map (fun r -> (fst r.OpeningPrice, r.Quantity)) |> weightedAverage |> Option.get
      let closingPrice = rs |> List.choose (fun r -> match r.ClosingPrice with Some p -> Some (fst p, r.Quantity) | _ -> None)
                            |> weightedAverage
                            |> Option.map (fun w -> (w, measure))
      let pnl = rs |> List.choose (fun r -> Option.map fst r.Pnl) |> function [] -> None | xs -> Some (List.sum xs, measure)
      let retn = closingPrice |> Option.map (fun (cp, _) -> let sg = if long then 1M else -1M
                                                            100M * sg * (cp - openingPrice) / openingPrice )
      {
        OpeningDate = date
        Account = acct
        Closed = closed
        Asset = asset
        Quantity = quantity
        Long = long
        OpeningPrice = (openingPrice, measure)
        ClosingDate = rs |> List.maxOf (fun r -> r.ClosingDate)
        ClosingPrice = closingPrice
        Pnl = pnl
        ReturnPct = retn
      }
    grouped |> List.map aggr

  let combined = trades |> List.collect createRow
                        |> iftrue request.ClosedOnly (fun xs -> xs |> List.filter (fun x -> x.Closed))
                        |> iftrue request.OpenOnly (fun xs -> xs |> List.filter (fun x -> not x.Closed))
                        |> iftrue request.Consolidated consolidate

  // Create table view
  let cs = [
    {Header = "Date"; Key=true}
    {Header = "Account"; Key=true}
    {Header = "ST"; Key=true}
    {Header = "Commodity"; Key=true}
    {Header = "Name"; Key=true}
    {Header = "Quantity"; Key=false}
    {Header = "O. Price"; Key=false}
    {Header = "Denom"; Key=false}
    {Header = "Age"; Key=false}
    {Header = "C. Date"; Key = false}
    {Header = "C. Price"; Key = false}
    {Header = "Unadj. PnL"; Key = false}
    {Header = "Unadj. %PnL"; Key = false}
    {Header = "Days"; Key = false}
  ]

  let createTableRow row =
    let (Commodity c) = row.Asset
    let (Commodity m) = snd row.OpeningPrice
    let age = (DateTime.Today - fst row.OpeningDate).Days
    let days = match row.ClosingDate with
                 | Some dt -> Number (decimal ((fst dt) - (fst row.OpeningDate)).Days, 0)
                 | None -> Empty
    [
      Date (fst row.OpeningDate)
      Text (getAccount row.Account)
      Text (if row.Closed then "X" else "O")
      Text c
      Text (nameFor row.Asset)
      Number (row.Quantity, 2)
      Number (fst row.OpeningPrice, quoteDPFor row.Asset)
      Text m
      Number (decimal age, 0)
      row.ClosingDate |> function | Some sq -> Date (fst sq) | _ -> Empty
      row.ClosingPrice |> function | Some o -> Number (fst o, quoteDPFor row.Asset) | _ -> Empty
      row.Pnl |> function | Some v -> Number (fst v, quoteDPFor row.Asset) | _ -> Empty
      row.ReturnPct |> function | Some v -> Number (v, 1) | _ -> Empty
      days
    ]

  let table = Table(cs, combined |> List.map createTableRow)
  renderer table