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

      let contraRAmountV = function | U d                 -> U (-d)
                                    | V (q, c)            -> V (-q, c)
                                    | Tf ((q, c), (p, m)) -> V (-q*p*multiplierOf c, m)
                                    | Th ((q, c), p)      -> V (-q*p*multiplierOf c, measureOf c)
                                    | Cr (_, b)           -> V b
                                    | Cl (a, _)           -> V a

      // is there a blank account for auto contra?
      let blanks, nonBlanks = xs |> List.partition (snd3 >> Option.isNone)
                                 |> (List.map fst3) *** (List.map (second3 Option.get))

      let collectWeightings (account, value, contraAccount) =
        // if this leg has it's own contra account, it automatically balances, it has zero weight to return
        match contraAccount with
          | NoCAccount ->
              match value with
                | U q                -> [q, tryCommodityOf account |> orGlobalCommodity]
                | V (q, c)           -> [q, c]
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
          function | U q -> Ve (q, tryCommodityOf account |> orGlobalCommodity)
                   | V v -> Ve v
                   | Tf (a, b) -> Tr (a, b)
                   | Th ((q, c), p) -> Tr ((q, c), (p, measureOf c))
                   | Cr (a, b) -> Xc (b, a)
                   | Cl (a, b) -> Xc (a, b)
        let cca = function | NoCAccount -> None | Self -> Some account | CAccount c -> Some c
        (account, value |> cvalue, cca contraAccount)

      let defaultContraAccount = List.tryHead blanks
      Some <| match List.length blanks, defaultContraAccount, List.length residual with
                | 0, _, 0       -> {Date = dt; Payee = py; Narrative = na; Postings = xs |> List.map (second3 Option.get >> convertXs)} |> Choice1Of2
                | 0, _, _       -> Choice2Of2 "Entry doesn't balance! Need a contra account but none specified."
                | 1, Some _ , 0 -> Choice2Of2 "Entry balances, but a contra account has been specified."
                | 1, Some ca, _ -> let zs = residual |> List.map (fun (c, v) -> (ca, V (-v, c), NoCAccount))
                                   {Date = dt; Payee = py; Narrative = na; Postings = nonBlanks @ zs |> List.map convertXs} |> Choice1Of2
                | _, _, _       -> Choice2Of2 "Entry has more than one default contra account, there should only be one."
  | _ -> None

let loadJournal filename =
  let elts = loadRJournal filename |> List.map stripComments
  // handle imports here ?//

  let headers = elts |> List.choose (function Header h -> Some h | _ -> None)
  let accountDecls = elts |> List.choose (function Account a -> Some (a.Account, a) | _ -> None)
                          |> Map.ofList
  let commodityDecls = elts |> List.choose (function RJournalElement.Commodity c -> Some (c.Symbol, c) | _ -> None)
                            |> Map.ofList

  let header = List.tryHead headers |> Option.defaultValue {Name = "Untitled"; Commodity = None; CapitalGains = None; Note = None}

  let entries = elts |> List.choose (balanceEntry header.Commodity accountDecls commodityDecls)

  let register = entries |> List.choose (function | Choice1Of2 x -> Some x
                                                  | Choice2Of2 s -> printfn "%s" s; None)
                         |> List.groupBy (fun e -> e.Date)
                         |> Map.ofList

  // Avengers assemble!
  {
    Meta = header
    AccountDecls = accountDecls
    CommodityDecls = commodityDecls
    Register = register
    Prices = Map.empty
    Splits = Map.empty
    Assertions = []
  }