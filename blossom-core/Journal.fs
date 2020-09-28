module Journal

open System
open Shared
open Types
open JournalParser

let marketAccount = Types.Account "_Market"
let conversionsAccount = Types.Account "_Conversions"

let internalDefaultCommodity = Types.Commodity "$"  // this is not a parsable value

let stripComments = function
  | Commented (elt, _) -> elt
  | elt -> elt

let balanceEntry gdc acctDecls commodDecls = function
  | Entry (dt, py, na, xs) ->
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

      let contraRAmountV = function | Un d                -> Un (-d)
                                    | Ve (q, c)           -> Ve (-q, c)
                                    | Tf ((q, c), (p, m)) -> Ve (-q*p*multiplierOf c, m)
                                    | Th ((q, c), p)      -> Ve (-q*p*multiplierOf c, measureOf c)
                                    | Cr (_, b)           -> Ve b
                                    | Cl (a, _)           -> Ve a

      // is there a blank account for auto contra?
      let blanks, nonBlanks = xs |> List.partition (fst >> snd3 >> Option.isNone)
                                 |> (List.map (fst >> fst3)) *** (List.map (fst >> second3 Option.get))

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
        let cvalue =
          function | Un q -> V (q, tryCommodityOf account |> orGlobalCommodity)
                   | Ve v -> V v
                   | Tf (a, b) -> T (a, b)
                   | Th ((q, c), p) -> T ((q, c), (p, measureOf c))
                   | Cr (a, b) -> X (b, a)
                   | Cl (a, b) -> X (a, b)
        let cca = function | NoCAccount -> None | Self -> Some account | CAccount c -> Some c
        (account, value |> cvalue, cca contraAccount)

      let defaultContraAccount = List.tryHead blanks
      Some <| match List.length blanks, defaultContraAccount, List.length residual with
                | 0, _, 0       -> {Date = dt; Payee = py; Narrative = na; Postings = xs |> List.map (fst >> second3 Option.get >> convertXs)} |> Choice1Of2
                | 0, _, _       -> Choice2Of2 "Entry doesn't balance! Need a contra account but none specified."
                | 1, Some _ , 0 -> Choice2Of2 "Entry balances, but a contra account has been specified."
                | 1, Some ca, _ -> let zs = residual |> List.map (fun (c, v) -> (ca, Ve (-v, c), NoCAccount))
                                   {Date = dt; Payee = py; Narrative = na; Postings = nonBlanks @ zs |> List.map convertXs} |> Choice1Of2
                | _, _, _       -> Choice2Of2 "Entry has more than one default contra account, there should only be one."
  | _ -> None

let loadJournal filename =
  let elts = loadRJournal filename |> List.map stripComments

  // handle imports here, we will need to rec the load function and combine results
  // will have to split this function later to handle
  let imports = elts |> List.choose (function Import i -> Some i | _ -> None)

  let header = elts |> List.choose (function Header h -> Some h | _ -> None)
                    |> List.tryHead
                    |> Option.defaultValue {Name = "Untitled"; Commodity = None; CapitalGains = None; Note = None}

  let accountDecls = elts |> List.choose (function Account a -> Some (a.Account, a) | _ -> None)
                          |> Map.ofList

  let commodityDecls = elts |> List.choose (function RJournalElement.Commodity c -> Some (c.Symbol, c) | _ -> None)
                            |> Map.ofList

  let register = elts |> List.choose (balanceEntry header.Commodity accountDecls commodityDecls)
                      |> List.choose (function | Choice1Of2 x -> Some x
                                               | Choice2Of2 s -> printfn "%s" s; None)
                      |> List.groupBy (fun e -> e.Date)
                      |> Map.ofList

  let prices = elts |> List.choose (function Prices (c, m, xs) -> Some ((c, m), xs) | _ -> None)
                    |> List.groupByApply fst snd
                    |> List.map (second (List.collect id >> Map.ofList))
                    |> Map.ofList

  let splits = elts |> List.choose (function Split (d, c, k1, k2) -> Some (c, (d, k1, k2)) | _ -> None)
                    |> List.groupByApply fst snd
                    |> Map.ofList

  let assertions = elts |> List.choose (function Assertion (d,a,v) -> Some (d,a,v) | _ -> None)

  // Avengers... assemble!
  {
    Meta = header
    AccountDecls = accountDecls
    CommodityDecls = commodityDecls
    Register = register
    Prices = prices
    Splits = splits
    Assertions = assertions
  }

let evaluateMovements journal =
  let register = journal.Register

  let multiplierOf c = journal.CommodityDecls |> Map.tryFind c
                                              |> Option.bind (fun d -> d.Multiplier)
                                              |> Option.defaultValue 1m

  let expandPosting (account, amount, caccount) =
    match amount with
      | V (qty, commodity) ->
          let contra = match caccount with Some ca -> [ca, -qty, commodity] | None -> []
          [account, qty, commodity] @ contra
      | T ((qty, commodity), (price, measure)) ->
          let cash = qty * price * multiplierOf commodity
          let contra = match caccount with Some ca -> [ca, -cash, measure] | None -> []
          [account, qty, commodity
           marketAccount, -qty, commodity
           marketAccount, cash, measure] @ contra
      | X ((qty1, commodity1), (qty2, commodity2)) ->
          let contra = match caccount with Some ca -> [ca, -qty2, commodity2] | None -> []
          [account, qty1, commodity1
           conversionsAccount, -qty1, commodity1
           conversionsAccount, qty2, commodity2] @ contra
  let expandEntry entry = entry.Postings |> List.collect expandPosting
  register |> Map.map (fun _ es -> List.collect expandEntry es)

let evaluateBalances journal =
  let movements = evaluateMovements journal |> Map.toList

  let f pre (dt, ms) =
    let applyMovement s (account, qty, commodity) =
      let before = s |> Map.tryFind account |> Option.defaultValue Map.empty
      let v = qty + (before |> Map.tryFind commodity |> Option.defaultValue 0m)
      s |> Map.add account (before |> Map.add commodity v)

    let result = ms |> List.fold applyMovement pre

    (dt, result), result

  movements |> List.mapFold f Map.empty |> fst |> Map.ofList

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
      | Some r -> es |> List.filter (fun e -> match e.Payee with | None -> true | Some p -> regexfilter r p)

  let narrativeFilter es =
    match request.narrative with
      | None -> es
      | Some r -> es |> List.filter (fun e -> regexfilter r e.Narrative)

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
      | true -> es |> narrativeFilter |> payeeFilter |> postingSemiFilter

  let register2 = register |> Map.map apply
  {journal with Register = register2}