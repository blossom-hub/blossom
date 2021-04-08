module Journal

open System
open System.IO
open Shared
open Types
open Definitions
open JournalParser
open Analysis

let getAccount (Types.Account (stub, _)) = stub

let getVirtualAccount (Types.Account (_, virt)) = virt

let stripVirtualAccount (Types.Account (stub, _)) = Types.Account (stub, None)

let splitAccounts (Types.Account (stub, virt)) =
  let f (a :string) = a.Split(':') |> List.ofArray
  f stub, virt

let joinAccounts elts virt=
  Types.Account (elts |> String.concat ":", virt)

// let stripComments = function
//   | Commented (elt, _) -> elt
//   | Entry (flagged, dt, payee, narrative, hs, xs) -> let zs = xs |> List.map    (function | PCommented (elt2, _) -> elt2 | elt2 -> elt2)
//                                                                  |> List.filter (function | PComment _ -> false | _ -> true)
//                                                      Entry (flagged, dt, payee, narrative, hs, zs)
//   | elt -> elt

let liftBasicEntry position date flagged dtransfer =
  // Temporarily look only at postings, come back to comments etc later
  let postings = dtransfer.Entries |> List.choose (function Posting (a,b,c) -> Some (a,b,c) | _ -> None)

  // Split postings up into categories before linking to their contra
  let emptyPostings     = postings |> List.choose (function (account, None, _)                        -> Some account | _ -> None)
  let unmatchedPostings = postings |> List.choose (function (account, Some value, None)               -> Some (account, V value) | _ -> None)
  let directedPostings  = postings |> List.choose (function (account, Some value, Some contraAccount) -> let caccount = match contraAccount with CS -> account | CV c -> c
                                                                                                         Some (account, V value, caccount) | _ -> None)

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
    | 0, _ -> $"" |> Choice2Of2
    | 1, _ -> let empty = List.exactlyOne emptyPostings
              unmatchedPostings |> List.map (fun (a, v) -> (a, v, empty))
                                |> lift
                                |> Choice1Of2
    | _, _ -> $"Only one empty balance entry is supported. {position}" |> Choice2Of2


let integrateRegister commodityDecls (transfers :Map<SQ, Entry list>) (analysedLots : AnalysedLot list) =
  // Build up transfer entries based on the lot details occuring in the InvestmentAnalysis
  // Notionals are not transferred if the traded instrument is MTM.
  let f1 (alot: AnalysedLot) =
    let mtm = commodityDecls |> Map.tryFind alot.Asset
                             |> Option.map (fun decl -> decl.Mtm)
                             |> Option.defaultValue false
    let openingNotional = first (fun p -> -p * decimal alot.Quantity) alot.PerUnitPrice
    let physicalTransfer = (marketAccount, V (-alot.Quantity, alot.Asset), alot.Account)
    let notionalTransfer = (alot.Settlement, V openingNotional, marketAccount)
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
                         let closingNotional = first (fun p -> p * mlot.Quantity) mlot.PerUnitPrice
                         let capitalGainsAccount = Option.defaultValue capitalGainsAccount mlot.CapitalGains
                         // Postings
                         // 1. Return the "physical asset"
                         // 2. Return the cash notional, if not mtm
                         // 3. Realise the pnl
                         let physicalTransfer2 = (marketAccount, V (mlot.Quantity, alot.Asset), alot.Account)
                         let notionalTransfer2 = (mlot.Settlement, V closingNotional, marketAccount)
                         let pnl = (fst mlot.PerUnitPrice - fst alot.PerUnitPrice) * mlot.Quantity, snd mlot.PerUnitPrice
                         let incomeTransfer2 = (marketAccount, V pnl, capitalGainsAccount)
                         let closePostings =
                          if mtm
                            then [physicalTransfer2; incomeTransfer2] @ mlot.Expenses
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

  let accountDecls = elts |> List.choose (function (_, Account a) -> Some (a.Account, a) | _ -> None)
                          |> Map.ofList

  let commodityDecls = elts |> List.choose (function (_, RJournalElement.Commodity c) -> Some (c.Symbol, c) | _ -> None)
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

  let investments = analyseInvestments trades dividends [] []
  let register = integrateRegister commodityDecls transfers investments

  // Avengers... assemble!
  {
    Meta = header
    AccountDecls = accountDecls
    CommodityDecls = commodityDecls
    InvestmentAnalysis = []
    Register = register
    Prices = prices
    Splits = splits
    Assertions = assertions
  }

let multiplierOf commodityDecls c =
  commodityDecls |> Map.tryFind c
                 |> Option.bind (fun d -> d.Multiplier)
                 |> Option.defaultValue 1m

let navIndicatorOf commodityDecls c =
  commodityDecls |> Map.tryFind c
                 |> Option.map (fun d -> if d.Mtm then 0m else 1m)
                 |> Option.defaultValue 1m

let expandPosting commodityDecls account amount caccount =
  match amount with
      | V (qty, commodity) ->
          [account, qty, commodity; caccount, -qty, commodity]
      | T ((qty, commodity), (price, measure), _) ->
          let cash = qty * price * multiplierOf commodityDecls commodity * navIndicatorOf commodityDecls commodity
          [account, qty, commodity
           marketAccount, -qty, commodity
           marketAccount, cash, measure
           caccount, -cash, measure]
      | X ((qty1, commodity1), (qty2, commodity2)) ->
          [account, qty1, commodity1
           conversionsAccount, -qty1, commodity1
           conversionsAccount, qty2, commodity2
           caccount, -qty2, commodity2]

let evaluateMovements commodityDecls register = // : Map<DateTime, (Account * decimal * Commodity) list> =
  let expandEntry entry = entry.Postings |> List.collect (fun (a,b,c) -> expandPosting commodityDecls a b c)
  register |> Map.map (fun _ es -> List.collect expandEntry es)

let inline summateAQCs iv (xs : (Account * decimal * Commodity) list) =
  let gk = match iv with | true -> (fst3 &&& thd3) | false -> ((fst3 >> stripVirtualAccount) &&& thd3)
  xs |> List.groupByApply gk (List.sumBy snd3)
     |> List.map (fun ((a,b),c) -> a,c,b)

let evaluateBalances journal =
  let movements = evaluateMovements journal.CommodityDecls journal.Register
  let msq = (DateTime.MinValue, None)
  // Group each date first, then calculate the cumulative balances per transition
  movements |> Map.map (fun _ es -> summateAQCs true es)
            |> Map.toList
            |> List.scan (fun (dp, bp) (dt, xs) -> (dt, summateAQCs true (bp @ xs))) (msq, [])
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
              | rs -> fun (acc, _, ca) ->
                        rs |> List.map (fun r -> getAccount acc |> regexfilter r || getAccount ca |> regexfilter r)
                           |> List.any id

    let g = match filter.VAccount with
              | None -> fun _ -> true
              | Some r -> fun (Types.Account (_, vacc), _, Types.Account (_, vca)) ->
                            let q1 = match vacc with Some s -> r = s | _ -> false
                            let q2 = match vca  with Some s -> r = s | _ -> false
                            q1 || q2

    let h = match filter.Commodities with
              | [] -> fun _ -> true
              | rs -> fun (_, amt, _) ->
                        rs |> List.map (fun r ->
                                match amt with
                                  | V (_, Types.Commodity c) -> regexfilter r c
                                  | T ((_, Types.Commodity c1), (_, Types.Commodity c2), _) -> regexfilter r c1 || regexfilter r c2
                                  | X ((_, Types.Commodity c1), (_, Types.Commodity c2))    -> regexfilter r c1 || regexfilter r c2)
                           |> List.any id
    es |> List.filter (fun e -> e.Postings |> List.exists (fun p -> f p && g p && h p))

  let apply sq es =
    match dateFilter sq with
      | false -> []
      | true -> es |> narrativeFilter |> payeeFilter |> tagFilter |> postingSemiFilter

  let register2 = register |> Map.map apply
  {journal with Register = register2}