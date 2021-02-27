module Journal

open System
open System.IO
open Shared
open Types
open Definitions
open JournalParser

let getAccount = function | Types.VirtualisedAccount (a, s) -> a | Types.Account a -> a

let getVirtualAccount = function | Types.VirtualisedAccount (_, s) -> Some s | Types.Account a -> None

let stripVirtualAccount =
  function | VirtualisedAccount (a, _) -> Types.Account a
           | x -> x

let splitAccounts =
  let f (a :string) = a.Split(':') |> List.ofArray
  function | VirtualisedAccount (a, s) -> AccountHierarchy (f a, Some s)
           | Types.Account a           -> AccountHierarchy (f a, None)

let joinAccounts (AccountHierarchy (xs, s)) =
  let stub = xs |> String.concat ":"
  match s with | Some q -> VirtualisedAccount (stub, q)
               | None   -> Types.Account stub

let stripComments = function
  | Commented (elt, _) -> elt
  | Entry (flagged, dt, payee, narrative, hs, xs) -> let zs = xs |> List.map    (function | PCommented (elt2, _) -> elt2 | elt2 -> elt2)
                                                                 |> List.filter (function | PComment _ -> false | _ -> true)
                                                     Entry (flagged, dt, payee, narrative, hs, zs)
  | elt -> elt

let balanceEntry gdc acctDecls commodDecls = function
  | (posn, Entry (flagged, dt, py, na, hs, xs)) ->
      // Helpers
      let measureOf commodity = commodDecls |> Map.tryFind commodity
                                            |> Option.bind (fun c -> c.Measure)
                                            |> function Some c -> c | None -> internalDefaultCommodity
      let multiplierOf commodity = commodDecls |> Map.tryFind commodity
                                               |> Option.bind (fun c -> c.Multiplier)
                                               |> Option.defaultValue 1m
      let tryCommodityOf account = acctDecls |> Map.tryFind account
                                             |> Option.bind (fun a -> a.Commodity)
      let orGlobalCommodity = Option.orElse gdc >> function Some c -> c | None -> internalDefaultCommodity

      let postings = xs |> List.choose (function | Posting (account, amount, contra) -> Some (account, amount, contra) | _ -> None)

      let contraWeightOf account ramount =
        match ramount with | Un q                  -> q, tryCommodityOf account |> orGlobalCommodity
                           | Ve (q, c)             -> q, c
                           | Tf ((q,c), (p, m), _) -> q*p*multiplierOf c, m
                           | Th ((q,c), p, _)      -> q*p*multiplierOf c, measureOf c
                           | Cr (_, (p, m))        -> p, m
                           | Cl ((q,c), _)         -> q, c

      let convert account ramount =
          match ramount with | Un q               -> V (q, tryCommodityOf account |> orGlobalCommodity)
                             | Ve (q, c)          -> V (q, c)
                             | Tf (a, b, ls)      -> T (a, b, ls)
                             | Th ((q, c), p, ls) -> T ((q, c), (p, measureOf c), ls)
                             | Cr (a, b)          -> X (a, b)
                             | Cl (a, b)          -> X (b, a)

      // Split postings up into categories before linking to their contra
      let emptyPostings = postings |> List.choose (function (account, None, _) -> Some account | _ -> None)
      let unmatchedPostings = postings |> List.choose (function (account, Some v, None) -> Some (account, v) | _ -> None)
      let selfMatchingPostings = postings |> List.choose (function (account, Some v, Some contraAccount) -> Some (account, convert account v, contraAccount) | _ -> None)

      (* NEW 12/02 --> fail if ambiguous (2x2 for example)
          1. Remove self-balancing
          2. if *1* missing account, auto-balance against this one
          3. if *2+* missing account, FAIL
          4. if *0* missing account, try to match on weight/weight basis, there must be 1 side with a single entry
                eg +2 USD +1 USD, vs -3 USD. otherwise fail
                check blancing
          5. oth fail
      *)

      let mkEntry ps = {
        Flagged = flagged
        Date = dt
        Payee = py
        Narrative = na
        HashTags = hs
        Postings = selfMatchingPostings @ ps
      }

      Some <| match List.length emptyPostings, List.length unmatchedPostings with
                | 0,0 -> mkEntry [] |> Choice1Of2
                | 1,0 -> $"Contra account is specified but not required. {posn}" |> Choice2Of2
                | 0,_ -> // break in to long/short by measure
                         let tag a v = let (q, c) = contraWeightOf a v in (a, v, q, c, Math.Sign q = 1)
                         let ls = unmatchedPostings |> List.map (uncurry tag)
                         let totals = ls |> List.groupByApply frh5 (List.sumBy thd5)
                         // firstly check if overall there is a balance overall
                         let unbalanced = totals |> List.filter (fun (c, q) -> q <> 0M) |> List.tryHead
                         match unbalanced with
                           | Some _ -> $"Entry does not balance, a contra account is needed but not supplied {posn}" |> Choice2Of2
                           | None -> // needs 1 -> n, or n -> 1 for matching to be automatically calculated now, else error (ambiguous matching)
                               let g = ls |> List.groupBy (frh5 &&& fih5)
                               let measures = List.map fst totals
                               let ones, multis = g |> List.partition (fun (_,vs) -> List.length vs = 1)
                               // look for the cases, and hook them up
                               // okay:     1 long / 1 short, 1 long / n short, n long / 1 short
                               // not okay: n long / n short
                               let onesMeasures = ones |> List.map (fst >> fst) |> List.distinct
                               let everyOne = measures |> List.all (flip List.contains onesMeasures)
                               match everyOne with
                                 | false -> $"Ambiguous posting match, each commodity measure should be 1-1 or many-to-1. {posn}" |> Choice2Of2
                                 | true ->
                                     let matchUp measure =
                                       let os = ones |> List.filter (fun x -> fst (fst x) = measure) |> List.collect snd
                                       let ms = multis |> List.filter (fun x -> fst (fst x) = measure) |> List.collect snd
                                       match List.length os with
                                         | 2 -> let (a, q, _, _, _) = List.head os
                                                let (c, _, _, _, _) = List.last os
                                                [(a, convert a q, c)]
                                         | 1 -> let (c, _, _, _, _) = List.head os
                                                ms |> List.map (fun (a, q, _, _, _) -> (a, convert a q, c))
                                         | _ -> failwith "Unexpected balancing problem matching measures in posting"
                                     measures |> List.collect matchUp |> mkEntry |> Choice1Of2
                | 1,_ -> let c = List.head emptyPostings
                         unmatchedPostings |> List.map (fun (a, v) -> (a, convert a v, c))
                                           |> mkEntry
                                           |> Choice1Of2
                | _,_ -> $"Only one empty balance entry is supported. {posn}" |> Choice2Of2

    | _ -> None

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
  let elts = loadRJournal filename |> List.map (second stripComments)

  let imports = elts |> List.choose (function (_, Import i) -> Some i | _ -> None)
  let cwd = Path.GetDirectoryName filename
  let imported = imports |> List.map (fun i -> match Path.IsPathRooted(i) with
                                                 | true -> loadJournal i
                                                 | false -> Path.Combine (cwd, i) |> loadJournal)
                         |> function | [] -> None
                                     | xs -> xs |> List.reduce mergeJournals |> Some

  let header = elts |> List.choose (function (_, Header h) -> Some h | _ -> None)
                    |> List.tryHead
                    |> Option.defaultValue {Name = "Untitled"; Commodity = None; CapitalGains = None; Note = None; Convention = None}

  let accountDecls = elts |> List.choose (function (_, Account a) -> Some (a.Account, a) | _ -> None)
                          |> Map.ofList

  let commodityDecls = elts |> List.choose (function (_, RJournalElement.Commodity c) -> Some (c.Symbol, c) | _ -> None)
                            |> Map.ofList

  let register = elts |> List.choose (balanceEntry header.Commodity accountDecls commodityDecls)
                      |> List.choose (function | Choice1Of2 x -> Some x
                                               | Choice2Of2 s -> printfn "%s" s; None)
                      |> List.groupBy (fun e -> e.Date)
                      |> Map.ofList

  let prices = elts |> List.choose (function (_, Prices (c, m, xs)) -> Some ((c, m), xs) | _ -> None)
                    |> List.groupByApply fst (List.collect snd >> Map.ofList)
                    |> Map.ofList

  let splits = elts |> List.choose (function (_, Split (d, c, k1, k2)) -> Some (c, (d, k1, k2)) | _ -> None)
                    |> List.groupByApply fst (List.map snd)
                    |> Map.ofList

  let assertions = elts |> List.choose (function (_, Assertion (d,a,v)) -> Some (d,a,v) | _ -> None)

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

let expandPosting commodityDecls account amount caccount =
  match amount with
      | V (qty, commodity) ->
          [account, qty, commodity; caccount, -qty, commodity]
      | T ((qty, commodity), (price, measure), _) ->
          let cash = qty * price * multiplierOf commodityDecls commodity
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

  let hashtagFilter (es : List<Entry>) =
    match filter.Hashtags with
      | [] -> es
      | rs -> es |> List.filter (fun e -> Set.intersect e.HashTags (Set.ofList rs) |> Set.isEmpty |> not)

  let postingSemiFilter (es : List<Entry>) =
    let f = match filter.Accounts with
              | [] -> fun _ -> true
              | rs -> fun (acc, _, ca) ->
                        rs |> List.map (fun r -> getAccount acc |> regexfilter r || getAccount ca |> regexfilter r)
                           |> List.any id

    let g = match filter.VAccount with
              | None -> fun _ -> true
              | Some r -> fun (acc, _, ca) ->
                            let q1 = match acc with | Types.Account _ -> true
                                                    | VirtualisedAccount (_, s) -> r = s
                            let q2 = match ca with | Types.Account _ -> true
                                                   | VirtualisedAccount (_, s) -> r = s
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
    es |> List.filter (fun e -> e.Postings |> List.exists (fun p -> f p && g p))

  let apply dt es =
    match dateFilter dt with
      | false -> []
      | true -> es |> narrativeFilter |> payeeFilter |> hashtagFilter |> postingSemiFilter

  let register2 = register |> Map.map apply
  {journal with Register = register2}