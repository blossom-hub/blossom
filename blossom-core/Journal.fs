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

      let defaultContraAccount = List.tryHead blanks
      match List.length blanks, defaultContraAccount, List.length residual with
        | 0, _, 0       -> Entry (dt, py, na, xs) |> Choice1Of2
        | 0, _, _       -> Choice2Of2 "Entry doesn't balance! Need a contra account but none specified."
        | 1, Some _ , 0 -> Choice2Of2 "Entry balances, but a contra account has been specified."
        | 1, Some ca, _ -> let ys = residual |> List.map (fun (c, v) -> (ca, Some <| V (-v, c), NoCAccount))
                           Entry (dt, py, na, xs @ ys) |> Choice1Of2
        | _, _, _       -> Choice2Of2 "Entry has more than one default contra account, there should only be one."
  | elt -> Choice1Of2 elt

let loadJournal filename =
  let elts = loadRJournal filename |> List.map stripComments
  // handle imports here ?//

  let headers = elts |> List.choose (function Header h -> Some h | _ -> None)
  let accountDecls = elts |> List.choose (function Account a -> Some (a.Account, a) | _ -> None)
                          |> Map.ofList
  let commodityDecls = elts |> List.choose (function RJournalElement.Commodity c -> Some (c.Symbol, c) | _ -> None)
                            |> Map.ofList

  let header = List.tryHead headers
  let globalDefaultCommodity = header |> Option.bind (fun h -> h.Commodity)

  let xyz = elts |> List.map (balanceEntry globalDefaultCommodity accountDecls commodityDecls)

  printfn "%A" xyz
  elts