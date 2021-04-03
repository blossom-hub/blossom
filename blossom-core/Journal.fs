module Journal

open System
open System.IO
open Shared
open Types
open Definitions
open JournalParser

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


// let balanceEntry commodities posn dt flagged py na hs xs =
//   let multiplierOf commodity = commodities |> Map.tryFind commodity
//                                            |> Option.bind (fun c -> c.Multiplier)
//                                            |> Option.defaultValue 1m
//   let navIndicatorOf commodity = commodities |> Map.tryFind commodity
//                                              |> Option.map (fun c -> if c.Mtm then 0m else 1m)
//                                              |> Option.defaultValue 1m
//   let postings = xs |> List.choose (function Posting (account, amount, contra) -> Some (account, amount, contra) | _ -> None)

//   let contraWeightOf account ramount =
//     match ramount with | Un q                  -> q, tryCommodityOf account |> orGlobalCommodity
//                        | Ve (q, c)             -> q, c
//                        | Tf ((q,c), (p, m), _) -> q*p*multiplierOf c * navIndicatorOf c, m
//                        | Th ((q,c), p, _)      -> q*p*multiplierOf c * navIndicatorOf c, measureOf c
//                        | Cr (_, (p, m))        -> p, m
//                        | Cl ((q,c), _)         -> q, c

//   let convert account ramount =
//       match ramount with | Un q               -> V (q, tryCommodityOf account |> orGlobalCommodity)
//                          | Ve (q, c)          -> V (q, c)
//                          | Tf (a, b, ls)      -> T (a, b, ls)
//                          | Th ((q, c), p, ls) -> T ((q, c), (p, measureOf c), ls)
//                          | Cr (a, b)          -> X (a, b)
//                          | Cl (a, b)          -> X (b, a)

//   // Split postings up into categories before linking to their contra
//   let emptyPostings = postings |> List.choose (function (account, None, _) -> Some account | _ -> None)
//   let unmatchedPostings = postings |> List.choose (function (account, Some v, None) -> Some (account, v) | _ -> None)
//   let selfMatchingPostings = postings |> List.choose (function (account, Some v, Some contraAccount) -> Some (account, convert account v, contraAccount) | _ -> None)

//   (* NEW 12/02 --> fail if ambiguous (2x2 for example)
//       1. Remove self-balancing
//       2. if *1* missing account, auto-balance against this one
//       3. if *2+* missing account, FAIL
//       4. if *0* missing account, try to match on weight/weight basis, there must be 1 side with a single entry
//             eg +2 USD +1 USD, vs -3 USD. otherwise fail
//             check blancing
//       5. oth fail
//   *)

//   let mkEntry ps = {
//     Flagged = flagged
//     Date = dt
//     Payee = py
//     Narrative = na
//     HashTags = hs
//     Postings = selfMatchingPostings @ ps
//   }

//   match List.length emptyPostings, List.length unmatchedPostings with
//     | 0,0 -> mkEntry [] |> Choice1Of2
//     | 1,0 -> $"Contra account is specified but not required. {posn}" |> Choice2Of2
//     | 0,_ -> // break in to long/short by measure
//              let tag a v = let (q, c) = contraWeightOf a v in (a, v, q, c, Math.Sign q = 1)
//              let ls = unmatchedPostings |> List.map (uncurry tag)
//              let totals = ls |> List.groupByApply frh5 (List.sumBy thd5)
//              // firstly check if overall there is a balance overall
//              let unbalanced = totals |> List.filter (fun (c, q) -> q <> 0M) |> List.tryHead
//              match unbalanced with
//                | Some _ -> $"Entry does not balance, a contra account is needed but not supplied {posn}" |> Choice2Of2
//                | None -> // needs 1 -> n, or n -> 1 for matching to be automatically calculated now, else error (ambiguous matching)
//                    let g = ls |> List.groupBy (frh5 &&& fih5)
//                    let measures = List.map fst totals
//                    let ones, multis = g |> List.partition (fun (_,vs) -> List.length vs = 1)
//                    // look for the cases, and hook them up
//                    // okay:     1 long / 1 short, 1 long / n short, n long / 1 short
//                    // not okay: n long / n short
//                    let onesMeasures = ones |> List.map (fst >> fst) |> List.distinct
//                    let everyOne = measures |> List.all (flip List.contains onesMeasures)
//                    match everyOne with
//                      | false -> $"Ambiguous posting match, each commodity measure should be 1-1 or many-to-1. {posn}" |> Choice2Of2
//                      | true ->
//                          let matchUp measure =
//                            let os = ones |> List.filter (fun x -> fst (fst x) = measure) |> List.collect snd
//                            let ms = multis |> List.filter (fun x -> fst (fst x) = measure) |> List.collect snd
//                            match List.length os with
//                              | 2 -> let (a, q, _, _, _) = List.head os
//                                     let (c, _, _, _, _) = List.last os
//                                     [(a, convert a q, c)]
//                              | 1 -> let (c, _, _, _, _) = List.head os
//                                     ms |> List.map (fun (a, q, _, _, _) -> (a, convert a q, c))
//                              | _ -> failwith "Unexpected balancing problem matching measures in posting"
//                          measures |> List.collect matchUp |> mkEntry |> Choice1Of2
//     | 1,_ -> let c = List.head emptyPostings
//              unmatchedPostings |> List.map (fun (a, v) -> (a, convert a v, c))
//                                |> mkEntry
//                                |> Choice1Of2
//     | _,_ -> $"Only one empty balance entry is supported. {posn}" |> Choice2Of2



// let sweepCapitalGains accountDecls commodityDecls defaultCGAccount entries =
//   // => beauty and efficiency can come later (once it works it can be simplied to avoid many loops)
//   (*
//       Sweep over all trading, to identify opening/closing transactions on a account/commodity[/measure? <- fx?] basis
//         -> TODO: cater for position transfers in determining matching lots
//         -> TODO: store all the analysis information for use later to avoid duplicate code analysis paths
//         -> TODO: virtualised accounts...


//       1. Firstly, identify which trades are Open/Close/Extend/Reduce (/Flip?) by creating running totals
//          Every transaction (call lot here) labelled upon loading with provided or auto name.
//          -> TODO Opening/Extend should be 1 label only in the parser, can check later (hard in parser to validate).
//          -> TODO Also confirm no duplicate names in use
//       2.
//   *)

//   // Step 0: label all the entries so that we can judiciously replace them later by key
//   let entries2 = entries |> List.map (fun e -> uid(), e)

//   // Step 1: Identify lot types
//   //  -> observe transaction per account/commodity[/measure <- fx?] basis
//   let trades = entries2 |> List.collect (fun (i, entry) -> entry.Postings |> List.choose (function (a1, T(c, p, ns), a2) -> Some ((i, entry.Date, c, p, a1, a2), ns) | _ -> None))
//                         |> List.sortBy (fun ((_, d, _, _, a1, _), _) -> (d, a1))

//   // all keys initialised with 0 balance to avoid the "missing or not" step below
//   let balances0 = trades |> List.map (fun ((_, _, (_,c), _, a1, _), _) -> a1, c) |> List.distinct |> List.map (fun x -> x, 0M) |> Map.ofList

//   // assume that we don't trade a 0 position
//   // -> TODO: flip trades (i.e. 100 long -> 100 short by trading -200 etc)
//   let f1 balances trade =
//     let ((i, dt, (q, c), p, a1, a2), ns) = trade
//     let currentBalance = balances |> Map.find (a1, c)
//     let startsZero = currentBalance = 0M
//     let startsLong = currentBalance > 0M
//     let positive = q >= 0M

//     let newBalance = currentBalance + q
//     let lt = match newBalance, startsZero, startsLong, positive with
//                | 0M, _,   _,     _     -> Close
//                | _, true, _,     _     -> Open
//                | _, _,    true,  true  -> Extend // Extend a long
//                | _, _,    false, false -> Extend // Extend a short
//                | _, _,    _,     _     -> Reduce
//     let updatedBalances = balances |> Map.add (a1, c) newBalance
//     (lt, q, c, ns, a1, fst trade), updatedBalances
//   let taggedTrades = trades |> List.mapFold f1 balances0
//                             |> fst
//                             |> List.mapi (fun i (lt, q, c, ns, a1, trade) -> i, lt, q, ns, trade)

//   // Step 2: Match the lots to calculate the closing lots
//   //  -> we want to match both directions, but this could be a second pass to store the meta
//   //  -> need to keep track of lots which are only _partially_ closed out.
//   //  -> TODO: different strategies, this is for FIFO; need to implement self-directed as an override
//   let openExtends, closeReduces = taggedTrades |> List.partition (fun (_, lt, _, _, _) -> match lt with | Open | Extend -> true | _ -> false)   // the list is [matching lot * matching qty]

//   // the outer loop folds over the individual close reduces
//   // the inner loop folds over the open extends [to match with the close reduce provided]
//   // mapFold :: ('state -> 'T -> 'Result * 'state) -> 'state -> 'T list -> ('Result list * 'state)
//   //  -> need to enumerate not just by date
//   // trade : idstring * DateTime * (decimal * Commodity) * Value * Account * Account
//   let f2 oeTrades crTrade =
//     let f3 crTrade_i oeTrade_j =
//       // sequence, lot type, remaining qty, commodity, lot names
//       let (cseq, clt, rcq, cns, ctr) = crTrade_i
//       let ((oseq, olt, roq, ons, otr), closures) = oeTrade_j
//       let (_, _, (_, cc), _, ca, _) = ctr
//       let (_, _, (_, oc), _, oa, _) = otr
//       // it is only matching if sequentually after, same commodity and same account
//       if (cseq > oseq) && (cc = oc) && (ca = oa)
//         then let amt = min (abs roq) (abs rcq)
//              let cq = decimal(sign rcq) * amt
//              let oq = decimal(sign roq) * amt
//              let closures2 = closures @ [ctr, otr, amt]
//              let crTrade_i2 = (cseq, clt, rcq - cq, cns, ctr)
//              let oeTrade_j2 = ((oseq, olt, roq - oq, ons, otr), closures2)
//              oeTrade_j2, crTrade_i2
//         else oeTrade_j, crTrade_i
//     let newPool, _ = List.mapFold f3 crTrade oeTrades
//     1, newPool

//   // closing lot name, closing qty, opening lot name
//   let matched = List.mapFold f2 (openExtends |> List.map (fun t -> (t, List.empty))) closeReduces
//                   |> snd
//                   |> List.collect snd

//   //   Postings: (Account * Amount * Account) list
//   let cg = matched |> List.map (fun (clt, olt, qty) -> let (_, _, (oq1, oc1), (oq2, om1), _, oa1) = olt
//                                                        let (ci, _, (cq1, _), (cq2, cm1), _, ca2) = clt
//                                                        let mm = commodityDecls |> Map.tryFind oc1
//                                                                                |> Option.bind (fun c -> c.Multiplier)
//                                                                                |> Option.defaultValue 1m
//                                                        // For now, always realise the whole profit on the sale, even for mtm items
//                                                        //  let nn = commodityDecls |> Map.tryFind oc1
//                                                        //                           |> Option.map (fun c -> if c.Mtm then 0m else 1m)
//                                                        //                           |> Option.defaultValue 1m
//                                                        let pnl = (cq2 - oq2) * qty * mm * decimal(Math.Sign(oq1))
//                                                        let cga = accountDecls |> Map.tryFind oa1
//                                                                               |> Option.bind (fun a -> a.CapitalGains)
//                                                                               |> Option.defaultValue (Option.defaultValue capitalGainsAccount defaultCGAccount)
//                                                        let isMtm = commodityDecls |> Map.tryFind oc1
//                                                                                   |> Option.map (fun c -> c.Mtm)
//                                                                                   |> Option.defaultValue false
//                                                        let income = (marketAccount, V (pnl, cm1), cga)
//                                                        if isMtm
//                                                         then let physical_income = (marketAccount, V (-pnl, cm1), ca2)
//                                                              (ci, [income; physical_income])
//                                                         else (ci, [income])
//                                                        )
//                    |> List.groupByApply fst (List.map snd >> List.concat)
//                    |> Map.ofList

//   let entries3 = entries2 |> List.map (fun (eid, e) -> let adjs = cg |> Map.tryFind eid
//                                                        match adjs with
//                                                          | Some xs -> {e with Postings = e.Postings @ xs}
//                                                          | None    -> e)

//   entries3

let private mergeJournals j1 j2 =
  {
    Meta = j2.Meta
    AccountDecls = Map.merge j1.AccountDecls j2.AccountDecls
    CommodityDecls = Map.merge j1.CommodityDecls j2.CommodityDecls
    Register = Map.mergeWith (fun _ v1 v2 -> v1 @ v2) j1.Register j2.Register
    Prices = Map.mergeWith (fun _ v1 v2 -> Map.merge v1 v2) j1.Prices j2.Prices
    Splits = Map.mergeWith (fun _ v1 v2 -> v1 @ v2) j1.Splits j2.Splits
    Assertions = j1.Assertions @ j2.Assertions
  }

let rec loadJournal filename =
  let elts = loadRJournal filename // |> List.map (second stripComments)

  // The sequence number (optional) is used and removed in this step. The result means that the order of the item is important.
  let items = elts |> List.choose (function (position, Item (sq, flagged, elt)) -> Some (position, sq, flagged, elt) | _ -> None)
                   |> List.sortBy snd4
                   |> List.map (fun (a,b,c,d) -> (a,fst b,c,d))

  let imports = elts |> List.choose (function (_, Import i) -> Some i | _ -> None)
  let cwd = Path.GetDirectoryName filename
  let imported = imports |> List.map (fun i -> match Path.IsPathRooted(i) with
                                                 | true -> loadJournal i
                                                 | false -> Path.Combine (cwd, i) |> loadJournal)
                         |> function | [] -> None
                                     | xs -> xs |> List.reduce mergeJournals |> Some

  let header = elts |> List.choose (function (_, Header h) -> Some h | _ -> None)
                    |> List.tryHead
                    |> Option.defaultValue {Name = "Untitled"; Commodity = None; Note = None; Convention = None}

  let accountDecls = elts |> List.choose (function (_, Account a) -> Some (a.Account, a) | _ -> None)
                          |> Map.ofList

  let commodityDecls = elts |> List.choose (function (_, RJournalElement.Commodity c) -> Some (c.Symbol, c) | _ -> None)
                            |> Map.ofList

  let register = items |> List.choose (function | (ps, dt, flagged, Transfer dtransfer) ->
                                                       Some (liftBasicEntry ps dt flagged dtransfer)
                                                | _ -> None)
                       |> List.choose (function | Choice1Of2 x -> Some x
                                                | Choice2Of2 s -> printfn "%s" s; None)
                    //   |> sweepCapitalGains accountDecls commodityDecls header.CapitalGains
                       |> List.groupBy (fun e -> e.Date)
                       |> Map.ofList

  let splits = items |> List.choose (function (_, dt, flagged, Split dsplit) -> Some (dsplit.Commodity, (dt, dsplit.K1, dsplit.K2)) | _ -> None)
                     |> List.groupByApply fst (List.map snd)
                     |> Map.ofList

  let assertions = items |> List.choose (function (_, dt, flagged, Assertion dassertion) -> Some (dt,dassertion.Account,dassertion.Value) | _ -> None)

  let prices = elts |> List.choose (function (_, Prices (c, m, xs)) -> Some ((c, m), xs) | _ -> None)
                    |> List.groupByApply fst (List.collect snd >> Map.ofList)
                    |> Map.ofList

  // Avengers... assemble!
  let journal = {
    Meta = header
    AccountDecls = accountDecls
    CommodityDecls = commodityDecls
    Register = register
    Prices = prices
    Splits = splits
    Assertions = assertions
  }

  match imported with
    | None    -> journal
    | Some js -> mergeJournals js journal

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

let evaluateMovements commodityDecls register : Map<DateTime, (Account * decimal * Commodity) list> =
  let expandEntry entry = entry.Postings |> List.collect (fun (a,b,c) -> expandPosting commodityDecls a b c)
  register |> Map.map (fun _ es -> List.collect expandEntry es)

let inline summateAQCs iv (xs : (Account * decimal * Commodity) list) =
  let gk = match iv with | true -> (fst3 &&& thd3) | false -> ((fst3 >> stripVirtualAccount) &&& thd3)
  xs |> List.groupByApply gk (List.sumBy snd3)
     |> List.map (fun ((a,b),c) -> a,c,b)

let evaluateBalances journal =
  let movements = evaluateMovements journal.CommodityDecls journal.Register

  // Group each date first, then calculate the cumulative balances per transition
  movements |> Map.map (fun _ es -> summateAQCs true es)
            |> Map.toList
            |> List.scan (fun (dp, bp) (dt, xs) -> (dt, summateAQCs true (bp @ xs))) (DateTime.MinValue, [])
            |> List.tail
            |> Map.ofList

let prefilter (filter: Filter) journal =
  let register = journal.Register

  let dateFilter dt =
    match filter.Timespan with
      | None -> true
      | Some (left, right) ->
          let q1 = match left  with | None -> true | Some (f, d0) -> (if f then (>=) else (>)) dt d0
          let q2 = match right with | None -> true | Some (f, dT) -> (if f then (<=) else (<)) dt dT
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

  let apply dt es =
    match dateFilter dt with
      | false -> []
      | true -> es |> narrativeFilter |> payeeFilter |> tagFilter |> postingSemiFilter

  let register2 = register |> Map.map apply
  {journal with Register = register2}