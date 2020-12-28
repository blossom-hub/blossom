module Journal

open System
open System.IO
open Shared
open Types
open JournalParser

let marketAccount = Types.Account "_Market"
let conversionsAccount = Types.Account "_Conversions"

let internalDefaultCommodity = Types.Commodity "$"  // this is not a parsable value

let splitAccounts (Types.Account a) = 
  a.Split(':') |> List.ofArray |> List.map Types.Account |> AccountHierarchy

let joinAccounts (Types.AccountHierarchy xs) = 
  xs |> List.map (function Types.Account a -> a) |> String.concat ":" |> Types.Account

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
      // is there a blank account for auto contra?
      let blanks, nonBlanks = postings |> List.partition (snd3 >> Option.isNone)
                                       |> (List.map fst3) *** (List.map (second3 Option.get))

      let collectWeightings (account, value, contraAccount) =
        // if this leg has it's own contra account, it automatically balances, it has zero weight to return
        match contraAccount with
          | NoCAccount ->
              match value with
                | Un q               -> [q, tryCommodityOf account |> orGlobalCommodity]
                | Ve (q, c)          -> [q, c]
                | Tf ((q,c), (p, m)) -> [q*p*multiplierOf c, m]
                | Th ((q,c), p)      -> [q*p*multiplierOf c, measureOf c]
                | Cr ((q,c), _)      -> [q, c]
                | Cl (_, (p, m))     -> [p, m]
          | _ -> []
      let ys = nonBlanks |> List.collect collectWeightings
      // now group up and check the result balancing (or not)
      let residual = ys |> List.groupBy snd
                        |> List.map (second (List.sumBy fst))
                        |> List.filter (fun (_,d) -> d <> 0M)

      let convertXs (account, value, contraAccount) =
        let cv =
          function | Un q -> V (q, tryCommodityOf account |> orGlobalCommodity)
                   | Ve v -> V v
                   | Tf (a, b) -> T (a, b)
                   | Th ((q, c), p) -> T ((q, c), (p, measureOf c))
                   | Cr (a, b) -> X (b, a)
                   | Cl (a, b) -> X (a, b)
        let cca = function | NoCAccount -> None | Self -> Some account | CAccount c -> Some c
        (account, cv value, cca contraAccount)

      let defaultContraAccount = List.tryHead blanks
      Some <| match List.length blanks, defaultContraAccount, List.length residual with
                | 0, _, 0       -> {Flagged = flagged; Date = dt; Payee = py; Narrative = na; HashTags = hs;
                                    Postings = postings |> List.map (second3 Option.get >> convertXs)} |> Choice1Of2
                | 0, _, _       -> sprintf "Entry doesn't balance! Need a contra account but none specified. %s" (posn.ToString()) |> Choice2Of2
                | 1, Some _ , 0 -> sprintf "Entry balances, but a contra account has been specified. %s" (posn.ToString()) |> Choice2Of2
                | 1, Some ca, _ -> let zs = residual |> List.map (fun (c, v) -> (ca, Ve (-v, c), NoCAccount))
                                   {Flagged = flagged; Date = dt; Payee = py; Narrative = na; HashTags = hs;
                                    Postings = nonBlanks @ zs |> List.map convertXs} |> Choice1Of2
                | _, _, _       -> sprintf "Entry has more than one default contra account, there should only be one. %s" (posn.ToString()) |> Choice2Of2
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
                    |> Option.defaultValue {Name = "Untitled"; Commodity = None; CapitalGains = None; Note = None}

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
          let contra = match caccount with Some ca -> [ca, -qty, commodity] | None -> []
          [account, qty, commodity] @ contra
      | T ((qty, commodity), (price, measure)) ->
          let cash = qty * price * multiplierOf commodityDecls commodity
          let contra = match caccount with Some ca -> [ca, -cash, measure] | None -> []
          [account, qty, commodity
           marketAccount, -qty, commodity
           marketAccount, cash, measure] @ contra
      | X ((qty1, commodity1), (qty2, commodity2)) ->
          let contra = match caccount with Some ca -> [ca, -qty2, commodity2] | None -> []
          [account, qty1, commodity1
           conversionsAccount, -qty1, commodity1
           conversionsAccount, qty2, commodity2] @ contra

let evaluateMovements commodityDecls register : Map<DateTime, (Account * decimal * Commodity) list> =
  let expandEntry entry = entry.Postings |> List.collect (fun (a,b,c) -> expandPosting commodityDecls a b c)
  register |> Map.map (fun _ es -> List.collect expandEntry es)

let inline summateAQCs xs =
  xs |> List.groupByApply (fst3 &&& thd3) (List.sumBy snd3)
     |> List.map (fun ((a,b),c) -> a,c,b)

let evaluateBalances journal =
  let movements = evaluateMovements journal.CommodityDecls journal.Register

  // Group each date first, then calculate the cumulative balances per transition
  movements |> Map.map (fun _ es -> summateAQCs es)
            |> Map.toList
            |> List.scan (fun (dp, bp) (dt, xs) -> (dt, summateAQCs (bp @ xs))) (DateTime.MinValue, [])
            |> List.tail
            |> Map.ofList

let prefilter request journal =
  let register = journal.Register

  let dateFilter dt =
    match request.between with
      | None -> true
      | Some (left, right) ->
          let q1 = match left  with | None -> true | Some (f, d0) -> (if f then (>=) else (>)) dt d0
          let q2 = match right with | None -> true | Some (f, dT) -> (if f then (<=) else (<)) dt dT
          q1 && q2

  let payeeFilter es =
    match request.payee with
      | None -> es
      | Some r -> es |> List.filter (fun e -> match e.Payee with | None -> false | Some p -> regexfilter r p)

  let narrativeFilter es =
    match request.narrative with
      | None -> es
      | Some r -> es |> List.filter (fun e -> regexfilter r e.Narrative)

  let hashtagFilter es =
    match Set.isEmpty request.hashtags with
      | true  -> es
      | false -> es |> List.filter (fun e -> Set.intersect e.HashTags request.hashtags |> Set.isEmpty |> not)

  let postingSemiFilter es =
    let f = match request.account with
              | None -> fun _ -> true
              | Some r -> fun (Types.Account a, _, ca) ->
                            let q1 = regexfilter r a
                            let q2 = match ca with Some (Types.Account c) -> regexfilter r c | _ -> true
                            q1 && q2
    let g = match request.commodity with
              | None -> fun _ -> true
              | Some r -> fun (_, amt, _) ->
                            match amt with
                              | V (_, Types.Commodity c) -> regexfilter r c
                              | T ((_, Types.Commodity c1), (_, Types.Commodity c2)) -> regexfilter r c1 || regexfilter r c2
                              | X ((_, Types.Commodity c1), (_, Types.Commodity c2)) -> regexfilter r c1 || regexfilter r c2
    es |> List.filter (fun e -> e.Postings |> List.exists (fun p -> f p && g p))

  let apply dt es =
    match dateFilter dt with
      | false -> []
      | true -> es |> narrativeFilter |> payeeFilter |> hashtagFilter |> postingSemiFilter

  let register2 = register |> Map.map apply
  {journal with Register = register2}