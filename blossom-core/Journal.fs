module Journal

open System
open System.IO
open Shared
open Types
open Helpers
open Definitions
open JournalParser
open Analysis

let liftBasicEntry position date flagged dtransfer =
  (* Highest priority is an individual split pair,
       followed by the residual post split to a specified contra,
       followed by the residual matched with an catch account
  *)
  let normalise (account, (q, m), contra) = if q < 0M then (contra, (-q, m), account) else (account, (q,m), contra)

  let postings = dtransfer.Entries |> List.choose (function Posting (a,b,c,_,d) -> Some (a,b,c,d) | _ -> None)

  let processSplits (account: Account) (qq: Value option) (contra: Contra option) splits =
    match splits with
      | [] -> [(account, qq, contra)]
      | xs -> let total = xs |> List.sumBy snd3
              let q, m = Option.get qq
              let splits2 = xs |> List.map (fun (splitContra, q1, _) -> (account, Some (-q1, m), Some (CV splitContra)))
              splits2 @ match q + total with
                          | 0M -> []
                          | x  -> [(account, Some (x, m), contra)]

  let flatPostings = postings |> List.collect (fun (a,b,c,d) -> processSplits a b c d)

  // Split postings up into categories before linking to their contra
  let emptyPostings     = flatPostings |> List.choose (function (account, None, _)                        -> Some account | _ -> None)
  let unmatchedPostings = flatPostings |> List.choose (function (account, Some value, None)               -> Some (account, value) | _ -> None)
  let directedPostings  = flatPostings |> List.choose (function (account, Some value, Some contraAccount) -> let caccount = match contraAccount with CS -> account | CV c -> c
                                                                                                             Some (account, value, caccount) | _ -> None)

  let lift liftedPostings = {
    Flagged = flagged
    Automatic = false
    Date = date
    Payee = dtransfer.Payee
    Narrative = dtransfer.Narrative
    Tags = dtransfer.Tags
    Postings = directedPostings @ liftedPostings |> List.map normalise
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
    let openingNotional = -alot.PerUnitPrice * decimal alot.Quantity * multiplier
    let physicalTransfer = (alot.Account, (alot.Quantity, alot.Asset), marketAccount)
    let notionalTransfer = (alot.Settlement, (openingNotional, alot.Measure), marketAccount)
    let openPostings =
      if mtm
        then [physicalTransfer] @ alot.Expenses
        else [physicalTransfer; notionalTransfer] @ alot.Expenses
    let (Types.Commodity commodity) = alot.Asset
    let openingEntry = {
      Flagged = false
      Automatic = true
      Date = alot.Date
      Payee = None
      Narrative = $"Open {commodity} x{alot.Quantity} @ {alot.PerUnitPrice}"
      Tags = Set.empty
      Postings = openPostings
    }
    let closingEntries =
      alot.Closings |> List.map (fun mlot ->
                         let closingNotional = mlot.PerUnitPrice * mlot.Quantity * multiplier
                         let capitalGainsAccount = Option.defaultValue capitalGainsAccount mlot.CapitalGains
                         // Postings
                         // 1. Return the "physical asset"
                         // 2. Return the cash notional, if not mtm
                         // 3. Realise the pnl
                         let physicalTransfer2 = (alot.Account, (mlot.Quantity, alot.Asset), marketAccount)
                         let notionalTransfer2 = (marketAccount, (closingNotional, alot.Measure), mlot.Settlement)
                         let incomeTransfer2 = (marketAccount, (mlot.UnadjustedPnL, alot.Measure), capitalGainsAccount)
                         let incomeTransfer3 = (mlot.Settlement, (mlot.UnadjustedPnL, alot.Measure), capitalGainsAccount)
                         let closePostings =
                          if mtm
                            then [physicalTransfer2; incomeTransfer3] @ mlot.Expenses
                            else [physicalTransfer2; incomeTransfer2; notionalTransfer2] @ mlot.Expenses
                         mlot.Date, {
                           Flagged = false
                           Automatic = true
                           Date = mlot.Date
                           Payee = None
                           Narrative = $"Close {commodity} x{mlot.Quantity} @ {mlot.PerUnitPrice}"
                           Tags = Set.empty
                           Postings = closePostings
                         })
    [alot.Date, openingEntry] @ closingEntries
  let tradingEntries = analysedLots |> List.collect f1 |> List.groupByApply fst (List.map snd) |> Map.ofList

  transfers |> Map.mergeWith (fun _ y z -> y @ z) tradingEntries

let rec loadJournal0 trace depth filename : (FParsec.Position * RJournalElement) list =
  if trace
    then let indent = String.replicate depth "  "
         printfn $"{indent}loading {filename}"
    else ()
  let elts0 = loadRJournal filename
  let cwd = Path.GetDirectoryName filename
  let imports = elts0 |> List.choose (function (_, Import i) -> Some (match Path.IsPathRooted i with
                                                                        | true  -> loadJournal0 trace (depth+1) i
                                                                        | false -> Path.Combine (cwd, i) |> loadJournal0 trace (depth+1))
                                                                | _ -> None)
                      |> List.concat
  elts0 @ imports

let loadJournal trace valuationDate filename =
  let elts = loadJournal0 trace 0 filename

  (* Valuation date filtering affects everything with a date
     - items is easy as everything is tagged with an SQ
     - others from elts need individual attention (as prices has the date inside)
  *)

  // Now process as one combined RJournal dataset
  let items = elts |> List.choose (function (position, Item (sq, flagged, elt)) -> Some (position, sq, flagged, elt) | _ -> None)
                   |> List.sortBy snd4
                   |> List.filter (fun quad -> (quad |> snd4 |> fst) <= valuationDate)

  let header = elts |> List.choose (function (_, Header h) -> Some h | _ -> None)
                    |> List.tryHead
                    |> Option.defaultValue {Name = "Untitled"; Commodity = None; Note = None; Convention = None}

  let accountDecls0 = elts |> List.choose (function (_, Account a) -> Some (a.Account, a) | _ -> None)
                           |> Map.ofList

  let commodityDecls0 = elts |> List.choose (function (_, RJournalElement.Commodity c) -> Some (c.Symbol, c) | _ -> None)
                             |> Map.ofList

  // handle each type of dated element here (aside from Comment2, these are only kept by the parser for admin purposes)
  let assertions = items |> List.choose (function (_, sq, flagged, Assertion dassertion) -> Some (fst sq, dassertion.Account, dassertion.Value) | _ -> None)

  let splits = items |> List.choose (function (_, sq, flagged, Split dsplit) -> Some (dsplit.Commodity, (fst sq, dsplit.K1, dsplit.K2)) | _ -> None)
                     |> List.groupByApply fst (List.map snd
                                                >> List.sortBy fst3
                                                >> fun ks -> List.scanBack (fun (dt, k1, k2) (_, k0) -> (dt, k0/k1 * k2)) ks (DateTime.MaxValue, 1M))
                     |> Map.ofList

  let prices0 = items |> List.choose (function (_, sq, flagged, Price dprice) -> Some ((dprice.Commodity, snd dprice.Price), [fst sq, fst dprice.Price]) | _ -> None)
  let prices1 = elts |> List.choose (function (_, Prices (c, m, xs)) -> Some ((c, m), xs) | _ -> None)
                     |> List.append prices0
                     |> List.groupByApply fst (List.collect snd >> Map.ofList)
                     |> Map.ofList
                     |> Map.map (fun ts -> Map.filter (fun dt _ -> dt <= valuationDate))

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

  let investments, prices = analyseInvestments commodityDecls0 trades dividends prices1 splits
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
  let impliedMeasures = investments |> List.groupByApply (fun i -> i.Asset) (fun xs -> xs |> List.head |> fun j -> j.Measure) |> Map.ofList
  let commodityDecls =
    let mk c = {Symbol = c; Measure = c; QuoteDP = None; Underlying = None; Name = None; Klass = None; Multiplier = None; Mtm = false; ExternalIdents = Map.empty}
    let setMeasure (decl: CommodityDecl) = {decl with Measure = impliedMeasures |> Map.tryFind decl.Symbol |> Option.defaultValue decl.Symbol}
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
    SplitKFactors = splits
    Assertions = assertions
  }

let expandPosting account (qty, commodity) caccount =
  [account, qty, commodity
   caccount, -qty, commodity]

let evaluateMovements includeVirtual register = // : Map<DateTime, (Account * decimal * Commodity) list> =
  let (|DV|) = if includeVirtual then id else dropVirtualAccount
  let expandEntry entry = entry.Postings |> List.collect (fun (DV a, b, DV c) -> expandPosting a b c)
  register |> Map.map (fun _ es -> List.collect expandEntry es)

let inline summateAQCs  (xs : (Account * decimal * Commodity) list) =
  xs |> List.groupByApply (fst3 &&& thd3) (List.sumBy snd3)
     |> List.map (fun ((a,b),c) -> a,c,b)

let evaluateBalances includeVirtual journal =
  let movements = evaluateMovements includeVirtual journal.Register
  let msq = (DateTime.MinValue, None)
  // Group each date first, then calculate the cumulative balances per transition
  movements |> Map.map (fun _ es -> summateAQCs es)
            |> Map.toList
            |> List.scan (fun (dp, bp) (dt, xs) -> (dt, summateAQCs (bp @ xs))) (msq, [])
            |> List.tail
            |> Map.ofList

let prefilter (filter: Filter) includeVirtual journal =
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
              | rs -> fun (FlattenAccount includeVirtual acc, _, FlattenAccount includeVirtual ca) ->
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

  let register2 = register |> Map.map apply |> Map.filter (fun _ v -> not (List.isEmpty v))
  {journal with Register = register2}