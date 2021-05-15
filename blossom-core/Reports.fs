module Reports

open System
open Analysis
open Types
open Shared
open Journal
open Tabular

// utilities
let groupTopn n =
  let f = splitAccounts >> List.take n >> (fun x -> joinAccounts x)
  List.map (first3 f) >> summateAQCs

let meta renderer request journal =
  let journalList = journal.Register |> Map.toList

  let commoditiesFromEntry entry = entry.Postings |> List.map (fun (_, (_, Commodity c), _) -> c)

  let accounts = journal.AccountDecls |> Map.toList
                                      |> List.map (function (Account acc, _) -> acc)
                                      |> Set.ofList

  let commodities = journal.CommodityDecls |> Map.toList
                                           |> List.map (function (Commodity c, _) -> c)
                                           |> Set.ofList

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
                                             |> List.map (fun (dt, Account a, q, Commodity c, actual) -> [Date dt; Text a; Text c; Number (q, 3); Number (actual, 3); Number (actual-q, 3)])
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
      | rs -> xs |> List.filter (fun (Account acc, _, _) ->
                                   rs |> List.map (fun r -> regexfilter r acc)
                                      |> List.any id)

  let commodityFilter xs =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, Commodity c) ->
                                  rs |> List.map (fun r -> regexfilter r c)
                                     |> List.any id)

  let measureFilter xs =
    match filter.Measures with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, c) ->
                                  let (Commodity measure) = journal.CommodityDecls |> Map.find c |> fun d -> d.Measure
                                  rs |> List.map (fun r -> regexfilter r measure)
                                     |> List.any id)

  let zeroFilter = List.filter (fun (_, q, _) -> q <> 0M)

  let result2 = result |> iftrue (not request.Flex) (accountFilter >> commodityFilter >> measureFilter)
                       |> iftrue request.GroupToTop (groupTopn 1)
                       |> iftrue request.HideZeros zeroFilter

  // temporarily make a table
  let gcn c = journal.CommodityDecls |> Map.tryFind c |> Option.bind (fun t -> t.Name) |> Option.defaultValue ""
  let table =
    let c = [{Header = "Account"; Key = true}; {Header = "Balance"; Key = false}; {Header ="Commodity"; Key = false}; {Header ="Name"; Key = false}]
    let d = result2 |> List.map (fun (a, q, c) -> (a, q, c))
                    |> summateAQCs
                    |> List.sortBy fst3
                    |> List.map (fun (Account a, q, Commodity c) -> [Text a; Number (q, 3); Text c; Text (gcn (Commodity c))])
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
      | rs -> xs |> List.filter (fun (_, _, _, _, Account acc, _, _) ->
                                   rs |> List.map (fun r -> regexfilter r acc)
                                      |> List.any id)

  let commodityFilter xs =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, _, _, _, _, Commodity c) ->
                                   rs |> List.map (fun r -> regexfilter r c)
                                      |> List.any id)

  let measureFilter xs =
    match filter.Measures with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, _, _, _, _, c) ->
                                  let (Commodity measure) = journal.CommodityDecls |> Map.find c |> fun d -> d.Measure
                                  rs |> List.map (fun r -> regexfilter r measure)
                                     |> List.any id)

  let flaggedFilter xs =
    xs |> List.filter (fun (x, _, _, _, _, _, _) -> x)

  let result2 = items |> iftrue (not request.Flex) (accountFilter >> commodityFilter >> measureFilter)
                      |> iftrue request.FlaggedOnly flaggedFilter

  let cs = [{Header = "Date"; Key=true}
            {Header = "F"; Key = true}
            {Header = "Payee"; Key = true}
            {Header = "Narrative"; Key =true}
            {Header = "Account"; Key = true}
            {Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let f2s = function true -> "*" | false -> ""
  let p2s = Option.defaultValue ""

  let createRow (f, d, p, n, Account a, q, Commodity c) =
    [Date (fst d); f2s f |> Text; p2s p |> Text; Text n; Text a; Number(q, 3); Text c]

  let data = result2 |> List.map createRow
  let table = Table (cs, data)

  renderer table

let balanceSeries renderer filter (request : SeriesRequest) journal =
  // do pre-filter
  let j2 = prefilter filter journal

  // define specific filters to crystalise the result
  let accountFilter xs =
    match filter.Accounts with
      | [] -> xs
      | rs -> xs |> List.filter (fun (Account acc, _, _) ->
                                   rs |> List.map (fun r -> regexfilter r acc)
                                      |> List.any id)

  let commodityFilter xs =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, Commodity c) ->
                                   rs |> List.map (fun r -> regexfilter r c)
                                      |> List.any id)

  let measureFilter xs =
    match filter.Measures with
      | [] -> xs
      | rs -> xs |> List.filter (fun (_, _, c) ->
                                  let (Commodity measure) = journal.CommodityDecls |> Map.find c |> fun d -> d.Measure
                                  rs |> List.map (fun r -> regexfilter r measure)
                                     |> List.any id)

  let zeroFilter = List.filter (fun (_, q, _) -> q <> 0M)

  let result = evaluateBalances j2 |> Map.toList
                  |> iftrue (not request.Flex) (List.map (second (accountFilter >> commodityFilter >> measureFilter)))
                  |> iftrue request.GroupToTop (List.map (second (groupTopn 1)))

  let dates = result |> List.map (fst >> fst)
  let left, right = dates |> (List.min &&& List.max)
  let schedule = makeSchedule request.Tenor left right |> Set.add left |> Set.toList

  let cs = [{Header = "Date"; Key=true}
            {Header = "Account"; Key = true}
            {Header = "Amount"; Key = false}
            {Header = "Commodity"; Key = false}]

  let p2s = Option.defaultValue ""
  let createRow dt (Account a, q, Commodity c) = [Date dt; Text a; Number (q, 3); Text c]

  match left = right with
    | true -> renderer (Table(cs, []))
    | false ->
        let balances = schedule |> List.map (fun dt -> dt, result |> List.takeWhile (fun (d,_) -> fst d <= dt) |> List.last |> snd)
        match request.Cumulative with
          | true -> let data= balances |> List.collect (fun (dt, xs) -> xs |> List.map (createRow dt))
                    Table (cs, data) |> renderer
          | false -> let balances2 = [(DateTime.MinValue, [])] @ balances
                                        |> List.pairwise
                                        |> List.map (fun ((_,xs), (dt,ys)) -> let xsn = xs |> List.map (fun (a, q ,c) -> (a, -q, c))
                                                                              let ds = summateAQCs (ys @ xsn)
                                                                              dt, ds)
                     let data = balances2 |> List.collect (fun (dt, xs) -> xs |> List.map (createRow dt))
                     Table (cs, data) |> renderer


type private InternalLotRow = {
  OpeningDate: SQ
  Account: Account
  Closed: bool
  Asset: Commodity
  Measure: Commodity
  Quantity: Decimal
  Long: bool
  OpeningPrice: decimal
  ClosingDate: SQ option
  ClosingPrice: decimal option
  Pnl: decimal option
  ReturnPct: decimal option
}

let filterInvestmentAnalysis (filter: Filter) (investments: OpeningTrade list) =

  let dateFilter (xs: OpeningTrade list) =
    let fil l lb r rb =
      xs |> List.filter (fun ot -> let dt, _ = ot.Date
                                   let q1 = (if lb then (>=) else (>)) dt l
                                   let q2 = (if rb then (<=) else (<)) dt r
                                   q1 && q2)
    match filter.Timespan with
      | None -> xs
      | Some (None, None) -> xs
      | Some (Some (b,dt), None) -> fil dt b DateTime.MaxValue true
      | Some (None, Some (b,dt)) -> fil DateTime.MinValue true dt b
      | Some (Some (lb, l), Some (rb, r)) -> fil l lb r rb

  let accountFilter (xs: OpeningTrade list) =
    match filter.Accounts with
      | [] -> xs
      | rs -> xs |> List.filter (fun ot -> let (Account a) = ot.Account
                                           rs |> List.map (fun r -> regexfilter r a) |> List.any id)

  let commodityFilter (xs: OpeningTrade list) =
    match filter.Commodities with
      | [] -> xs
      | rs -> xs |> List.filter (fun ot -> let (Commodity c) = ot.Asset
                                           rs |> List.map (fun r -> regexfilter r c) |> List.any id)

  let measureFilter (xs: OpeningTrade list) =
    match filter.Measures with
      | [] -> xs
      | rs -> xs |> List.filter (fun ot -> let (Commodity measure) = ot.Measure
                                           rs |> List.map (fun r -> regexfilter r measure)
                                              |> List.any id)

  // analysis has already completed, so most work here is the filter application
  investments |> dateFilter
              |> accountFilter
              |> commodityFilter
              |> measureFilter

let lotAnalysis renderer (filter: Filter) (request: LotRequest) journal =
  let quoteDPFor commodity = journal.CommodityDecls |> Map.tryFind commodity
                                                    |> Option.bind (fun t -> t.QuoteDP)
                                                    |> Option.defaultValue 3
  let nameFor commodity = journal.CommodityDecls |> Map.tryFind commodity
                                                 |> Option.bind (fun t -> t.Name)
                                                 |> Option.defaultValue ""

  let trades = filterInvestmentAnalysis filter journal.InvestmentAnalysis

  // create an anon-record representing the row data before
  // transforming to the concrete type, to make it easier to track
  // calcs and types
  let createRow (ot: OpeningTrade) =
    let row (ct: ClosingTrade option) =
      let rowQty = ct |> Option.map (fun c -> -c.Quantity) |> Option.defaultValue ot.Quantity
      let retn = ct |> Option.map (fun c -> let cp = c.PerUnitPrice
                                            let op = ot.PerUnitPrice
                                            let sg = if rowQty > 0M then 1M else -1M
                                            100M * sg * (cp - op) / op)
      {
        OpeningDate = ot.Date
        Account = ot.Account
        Closed = match ct with Some _ -> true | _ -> false
        Asset = ot.Asset
        Measure = ot.Measure
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
    let grouped = rows |> List.groupBy (fun r -> (r.OpeningDate, r.Long, r.Account, r.Closed, r.Asset, r.Measure))
    let aggr ((date, long, acct, closed, asset, measure), rs) =
      let quantity = rs |> List.sumBy (fun r -> r.Quantity)
      let openingPrice = rs |> List.map (fun r -> (r.OpeningPrice, r.Quantity)) |> weightedAverage |> Option.get
      let closingPrice = rs |> List.choose (fun r -> match r.ClosingPrice with Some p -> Some (p, r.Quantity) | _ -> None)
                            |> weightedAverage
      let pnl = rs |> List.choose (fun r -> r.Pnl) |> function [] -> None | xs -> Some (List.sum xs)
      let retn = closingPrice |> Option.map (fun cp -> let sg = if long then 1M else -1M
                                                       100M * sg * (cp - openingPrice) / openingPrice )
      {
        OpeningDate = date
        Account = acct
        Closed = closed
        Asset = asset
        Measure = measure
        Quantity = quantity
        Long = long
        OpeningPrice = openingPrice
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
    {Header = "Ms"; Key=false}
    {Header = "Age"; Key=false}
    {Header = "C. Date"; Key = false}
    {Header = "C. Price"; Key = false}
    {Header = "Unadj. PnL"; Key = false}
    {Header = "Unadj. %PnL"; Key = false}
    {Header = "Days"; Key = false}
  ]

  let createTableRow row =
    let (Account a) = row.Account
    let (Commodity c) = row.Asset
    let (Commodity m) = row.Measure
    let age = (DateTime.Today - fst row.OpeningDate).Days
    let days = match row.ClosingDate with
                 | Some dt -> Number (decimal ((fst dt) - (fst row.OpeningDate)).Days, 0)
                 | None -> Empty
    [
      Date (fst row.OpeningDate)
      Text a
      Text (if row.Closed then "X" else "O")
      Text c
      Text (nameFor row.Asset)
      Number (row.Quantity, 2)
      Number (row.OpeningPrice, quoteDPFor row.Asset)
      Text m
      Number (decimal age, 0)
      row.ClosingDate |> function | Some sq -> Date (fst sq) | _ -> Empty
      row.ClosingPrice |> function | Some o -> Number (o, quoteDPFor row.Asset) | _ -> Empty
      row.Pnl |> function | Some v -> Number (v, quoteDPFor row.Asset) | _ -> Empty
      row.ReturnPct |> function | Some v -> Number (v, 1) | _ -> Empty
      days
    ]

  let table = Table(cs, combined |> List.map createTableRow)
  renderer table


let holdingsAnalysis renderer (filter: Filter) (journal: Journal) =
  let trades = filterInvestmentAnalysis filter journal.InvestmentAnalysis

  let nameFor commodity = journal.CommodityDecls |> Map.tryFind (Commodity commodity)
                                                 |> Option.bind (fun t -> t.Name)
                                                 |> Option.defaultValue ""

  let fn (ot: OpeningTrade list) =
    let averageOpeningPrice = ot |> List.map (fun o -> (o.PerUnitPrice, o.OpenQuantity)) |> weightedAverage |> Option.get
    let openQuantity = ot |> List.sumBy (fun o -> o.OpenQuantity)
    let unrealised = ot |> List.sumBy (fun o -> o.UnrealisedPnL)
    let lastPrice = ot |> List.head |> fun o -> snd o.LastUnitPrice
    openQuantity, averageOpeningPrice, lastPrice, unrealised

  let trades =
    journal.InvestmentAnalysis
    |> List.filter (fun ot -> ot.OpenQuantity <> 0M)
    |> List.groupByApply (fun ot -> (ot.Account, ot.Asset, ot.Measure)) fn
    |> List.sortBy fst

  let cs = [
    {Header = "Account"; Key=true}
    {Header = "Commodity"; Key=true}
    {Header = "Name"; Key=true}
    {Header = "Ms"; Key=true}
    {Header = "Position"; Key=false}
    {Header = "Avg. Price"; Key=false}
    {Header = "Last Price"; Key=false}
    {Header = "Unrl. PnL"; Key=false}
  ]

  let createRow ((Account account, Commodity commodity, Commodity measure), (posn, aop, lp, upnl)) = [
    Text account
    Text commodity
    Text (nameFor commodity)
    Text measure
    Number (posn, 3)
    Number (aop, 3)
    Number (lp, 3)
    Number (upnl, 3)
  ]

  let table = Table (cs, trades |> List.map createRow)
  renderer table