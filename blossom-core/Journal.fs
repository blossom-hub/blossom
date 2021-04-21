module Journal

open System
open System.IO
open Shared
open Types
open Definitions
open JournalParser
open Analysis

let splitAccounts (Types.Account stub) =
  let f (a :string) = a.Split(':') |> List.ofArray
  f stub

let joinAccounts elts =
  Types.Account (elts |> String.concat ":")

// let stripComments = function
//   | Commented (elt, _) -> elt
//   | Entry (flagged, dt, payee, narrative, hs, xs) -> let zs = xs |> List.map    (function | PCommented (elt2, _) -> elt2 | elt2 -> elt2)
//                                                                  |> List.filter (function | PComment _ -> false | _ -> true)
//                                                      Entry (flagged, dt, payee, narrative, hs, zs)
//   | elt -> elt

let liftBasicEntry position date flagged dtransfer =
  // Temporarily look only at postings, come back to comments etc later
  let postings = dtransfer.Entries |> List.choose (function Posting (a,b,c,_) -> Some (a,b,c) | _ -> None)

  // Split postings up into categories before linking to their contra
  let emptyPostings     = postings |> List.choose (function (account, None, _)                        -> Some account | _ -> None)
  let unmatchedPostings = postings |> List.choose (function (account, Some value, None)               -> Some (account, value) | _ -> None)
  let directedPostings  = postings |> List.choose (function (account, Some value, Some contraAccount) -> let caccount = match contraAccount with CS -> account | CV c -> c
                                                                                                         Some (account, value, caccount) | _ -> None)

  let lift liftedPostings = {
    Flagged = flagged
    Date = date
    Payee = dtransfer.Payee
    Narrative = dtransfer.Narrative
    Tags = dtransfer.Tags
    Postings = directedPostings @ liftedPostings
  }

  // todo enhance the error propagation here to keep it coming
  match List.length emptyPostings, List.length unmatchedPostings with
    | 0, 0 -> Choice1Of2 (lift [])
    | 1, 0 -> $"Contra account is specified, but not required. {position}" |> Choice2Of2
    | 0, _ -> $"Empty vs. unmatched issue 1 {position}" |> Choice2Of2
    | 1, _ -> let empty = List.exactlyOne emptyPostings
              unmatchedPostings |> List.map (fun (a, v) -> (a, v, empty))
                                |> lift
                                |> Choice1Of2
    | _, _ -> $"Only one empty balance entry is supported. {position}" |> Choice2Of2

let integrateRegister commodityDecls (transfers :Map<SQ, Entry list>) analysedLots =
  // Build up transfer entries based on the lot details occuring in the InvestmentAnalysis
  // Notionals are not transferred if the traded instrument is MTM.
  let f1 (alot: OpeningTrade) =
    let multiplier = multiplierOf commodityDecls alot.Asset
    let mtm = isMtm commodityDecls alot.Asset
    let openingNotional = first (fun p -> -p * decimal alot.Quantity * multiplier) alot.PerUnitPrice
    let physicalTransfer = (alot.Account, (alot.Quantity, alot.Asset), marketAccount)
    let notionalTransfer = (alot.Settlement, openingNotional, marketAccount)
    let openPostings =
      if mtm
        then [physicalTransfer] @ alot.Expenses
        else [physicalTransfer; notionalTransfer] @ alot.Expenses
    let openingEntry = {
      Flagged = false
      Date = alot.Date
      Payee = None
      Narrative = ""
      Tags = Set.empty
      Postings = openPostings
    }
    let closingEntries =
      alot.Closings |> List.map (fun mlot ->
                         let closingNotional = first (fun p -> p * mlot.Quantity * multiplier) mlot.PerUnitPrice
                         let capitalGainsAccount = Option.defaultValue capitalGainsAccount mlot.CapitalGains
                         // Postings
                         // 1. Return the "physical asset"
                         // 2. Return the cash notional, if not mtm
                         // 3. Realise the pnl
                         let physicalTransfer2 = (alot.Account, (mlot.Quantity, alot.Asset), marketAccount)
                         let notionalTransfer2 = (marketAccount, closingNotional, mlot.Settlement)
                         let incomeTransfer2 = (marketAccount, mlot.UnadjustedPnL, capitalGainsAccount)
                         let incomeTransfer3 = (mlot.Settlement, mlot.UnadjustedPnL, capitalGainsAccount)
                         let closePostings =
                          if mtm
                            then [physicalTransfer2; incomeTransfer3] @ mlot.Expenses
                            else [physicalTransfer2; incomeTransfer2; notionalTransfer2] @ mlot.Expenses
                         mlot.Date, {
                           Flagged = false
                           Date = mlot.Date
                           Payee = None
                           Narrative = ""
                           Tags = Set.empty
                           Postings = closePostings
                         })
    [alot.Date, openingEntry] @ closingEntries
  let tradingEntries = analysedLots |> List.collect f1 |> List.groupByApply fst (List.map snd) |> Map.ofList

  transfers |> Map.mergeWith (fun _ y z -> y @ z) tradingEntries

let loadJournal filename =
  // load the journal, find imports, load imports, combine, process as one
  let elts0 = loadRJournal filename
  let cwd = Path.GetDirectoryName filename
  let imports = elts0 |> List.choose (function (_, Import i) -> Some (match Path.IsPathRooted i with
                                                                        | true  -> loadRJournal i
                                                                        | false -> Path.Combine (cwd, i) |> loadRJournal )
                                                                | _ -> None)
                      |> List.concat
  let elts = elts0 @ imports

  // Now process as one combined RJournal dataset
  let items = elts |> List.choose (function (position, Item (sq, flagged, elt)) -> Some (position, sq, flagged, elt) | _ -> None)
                   |> List.sortBy snd4

  let header = elts |> List.choose (function (_, Header h) -> Some h | _ -> None)
                    |> List.tryHead
                    |> Option.defaultValue {Name = "Untitled"; Commodity = None; Note = None; Convention = None}

  let accountDecls0 = elts |> List.choose (function (_, Account a) -> Some (a.Account, a) | _ -> None)
                           |> Map.ofList

  let commodityDecls0 = elts |> List.choose (function (_, RJournalElement.Commodity c) -> Some (c.Symbol, c) | _ -> None)
                             |> Map.ofList

  // handle each type of dated element here (aside from Comment2, these are only kept by the parser for admin purposes)
  let assertions = items |> List.choose (function (_, sq, flagged, Assertion dassertion) -> Some (fst sq, dassertion.Account, dassertion.Value) | _ -> None)

  let prices = elts |> List.choose (function (_, Prices (c, m, xs)) -> Some ((c, m), xs) | _ -> None)
                    |> List.groupByApply fst (List.collect snd >> Map.ofList)
                    |> Map.ofList

  let splits = items |> List.choose (function (_, sq, flagged, Split dsplit) -> Some (dsplit.Commodity, (fst sq, dsplit.K1, dsplit.K2)) | _ -> None)
                     |> List.groupByApply fst (List.map snd)
                     |> Map.ofList

  let transfers = items |> List.choose (function | (ps, sq, flagged, Transfer dtransfer) -> Some (liftBasicEntry ps sq flagged dtransfer)
                                                 | _ -> None)
                        |> List.choose (function | Choice1Of2 x -> Some x
                                                 | Choice2Of2 s -> printfn "%s" s; None)
                        |> List.groupBy (fun e -> e.Date)
                        |> Map.ofList

  let dividends = items |> List.choose (function | (ps, sq, flagged, Dividend ddividend) -> Some (ps, sq, flagged, ddividend)
                                                 | _ -> None)

  let trades = items |> List.choose (function | (ps, sq, flagged, Trade dtrade) -> Some (ps, sq, flagged, dtrade)
                                              | _ -> None)

  let investments = analyseInvestments commodityDecls0 trades dividends [] []
  let register = integrateRegister commodityDecls0 transfers investments

  // collect all accounts and merge with decls
  let accountDecls =
    let mk acc = {Account = acc; ValuationMode = Latest; Commodity = None; Note = None}
    register |> Map.toList
             |> List.collect (fun (_, xs) -> xs |> List.collect (fun e -> e.Postings |> List.collect (fun (a,_,c) -> [a, mk a; c, mk c])))
             |> List.distinct
             |> Map.ofList
             |> Map.merge accountDecls0

  // collect all commodities and merge with decls
  // implied measures
  let impliedMeasures = investments |> List.groupByApply (fun i -> i.Asset) (fun xs -> xs |> List.head |> fun j -> snd j.PerUnitPrice) |> Map.ofList
  let commodityDecls =
    let mk c = {Symbol = c; Measure = c; QuoteDP = None; Underlying = None; Name = None; Klass = None; Multiplier = None; Mtm = false; ExternalIdents = Map.empty}
    let setMeasure decl = {decl with Measure = impliedMeasures |> Map.tryFind decl.Symbol |> Option.defaultValue decl.Symbol}
    register |> Map.toList
             |> List.collect (fun (_, xs) -> xs |> List.collect (fun e -> e.Postings |> List.collect (fun (_,(_,c),_) -> [c, mk c])))
             |> List.distinct
             |> Map.ofList
             |> Map.merge commodityDecls0
             |> Map.map (fun _k v -> setMeasure v)

  // Avengers... assemble!
  {
    Meta = header
    AccountDecls = accountDecls
    CommodityDecls = commodityDecls
    InvestmentAnalysis = investments
    Register = register
    Prices = prices
    Splits = splits
    Assertions = assertions
  }

let expandPosting account (qty, commodity) caccount =
  [account, qty, commodity
   caccount, -qty, commodity]

let evaluateMovements register = // : Map<DateTime, (Account * decimal * Commodity) list> =
  let expandEntry entry = entry.Postings |> List.collect (fun (a,b,c) -> expandPosting a b c)
  register |> Map.map (fun _ es -> List.collect expandEntry es)

let inline summateAQCs  (xs : (Account * decimal * Commodity) list) =
  xs |> List.groupByApply (fst3 &&& thd3) (List.sumBy snd3)
     |> List.map (fun ((a,b),c) -> a,c,b)

let evaluateBalances journal =
  let movements = evaluateMovements journal.Register
  let msq = (DateTime.MinValue, None)
  // Group each date first, then calculate the cumulative balances per transition
  movements |> Map.map (fun _ es -> summateAQCs es)
            |> Map.toList
            |> List.scan (fun (dp, bp) (dt, xs) -> (dt, summateAQCs (bp @ xs))) (msq, [])
            |> List.tail
            |> Map.ofList

let prefilter (filter: Filter) journal =
  let register = journal.Register

  let dateFilter sq =
    match filter.Timespan with
      | None -> true
      | Some (left, right) ->
          let q1 = match left  with | None -> true | Some (f, d0) -> (if f then (>=) else (>)) (fst sq) d0
          let q2 = match right with | None -> true | Some (f, dT) -> (if f then (<=) else (<)) (fst sq) dT
          q1 && q2

  let payeeFilter (es : List<Entry>) =
    match filter.Payees with
      | [] -> es
      | rs -> es |> List.filter (fun e -> match e.Payee with
                                            | None   -> false
                                            | Some p -> List.map (fun r -> regexfilter r p) rs |> List.any id)

  let narrativeFilter (es : List<Entry>) =
    match filter.Narrative with
      | None -> es
      | Some r -> es |> List.filter (fun e -> regexfilter r e.Narrative)

  let tagFilter (es : List<Entry>) =
    match filter.Tags with
      | [] -> es
      | rs -> es |> List.filter (fun e -> Set.intersect e.Tags (Set.ofList rs) |> Set.isEmpty |> not)

  let postingSemiFilter (es : List<Entry>) =
    let f = match filter.Accounts with
              | [] -> fun _ -> true
              | rs -> fun (Types.Account acc, _, Types.Account ca) ->
                        rs |> List.map (fun r -> regexfilter r acc || regexfilter r ca)
                           |> List.any id

    let h = match filter.Commodities with
              | [] -> fun _ -> true
              | rs -> fun (_, (_, Types.Commodity c), _) ->
                        rs |> List.map (fun r -> regexfilter r c)
                           |> List.any id

    let g = match filter.Measures with
              | [] -> fun _ -> true
              | rs -> fun (_, (_, c), _) ->
                        let (Types.Commodity measure) = journal.CommodityDecls |> Map.find c |> fun d -> d.Measure
                        rs |> List.map (fun r -> regexfilter r measure)
                           |> List.any id

    es |> List.filter (fun e -> e.Postings |> List.exists (fun p -> f p && h p && g p))

  let apply sq es =
    match dateFilter sq with
      | false -> []
      | true -> es |> narrativeFilter |> payeeFilter |> tagFilter |> postingSemiFilter

  let register2 = register |> Map.map apply
  {journal with Register = register2}