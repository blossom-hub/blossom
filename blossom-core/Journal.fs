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
                                            |> function Some c -> c | None -> raise (ArgumentException "No matched commodity definition")
      let multiplierOf commodity = commodDecls |> Map.tryFind commodity
                                               |> Option.bind (fun c -> c.Multiplier)
                                               |> Option.defaultValue 1m
      let tryCommodityOf account = acctDecls |> Map.tryFind account
                                             |> Option.bind (fun a -> a.Commodity)
      let orGlobalCommodity = Option.orElse gdc >> function Some c -> c | None -> raise (ArgumentException "No global default commodity for fallback")

      let contraRAmountV = function | U d                 -> U (-d)
                                    | V (q, c)            -> V (-q, c)
                                    | Tf ((q, c), (p, m)) -> V (-q*p*multiplierOf c, m)
                                    | Th ((q, c), p)      -> V (-q*p*multiplierOf c, measureOf c)
                                    | Cr (_, b)           -> V b
                                    | Cl (a, _)           -> V a

      try
          // is there a blank account for auto contra?
          let blanks, nonBlanks = xs |> List.partition (snd3 >> Option.isNone)
                                     |> (List.map fst3) *** (List.map (second3 Option.get))

          if List.length blanks > 1
            then raise (ArgumentException "There can only be one default blank account")
            else ()

          let defaultContraAccount = List.tryHead blanks

          // create the contra element y for each x in xs; only need to consider the cash/measure
          // amounts at this stage (the other commodity parts are auto generated - they would balance)
          let createContraV (account, value, cAccount) =
            let contraAccount = Option.orElse defaultContraAccount cAccount
            let contraAccountCommodity () = contraAccount |> Option.bind tryCommodityOf |> orGlobalCommodity
            let getContraAccount () = match contraAccount with Some a -> a | None -> raise (ArgumentException "Must provide contra account for entry")
            match value with
              | U d -> let c = tryCommodityOf account |> Option.defaultValue (contraAccountCommodity())
                       [account, d, c; getContraAccount(), -d, c]
              | V (q, c) -> [account, q, c; getContraAccount(), -q, c]
              | Tf ((q,c), (p, m)) ->
                  let account2 = Option.defaultValue account contraAccount
                  let v = q*p*multiplierOf c
                  [account2, -v, m; marketAccount, v, m]
              | Th ((q,c), p) ->
                  let account2 = Option.defaultValue account contraAccount
                  let m = measureOf c
                  let v = q*p*multiplierOf c
                  [account2, -v, m; marketAccount, v, m]
              | Cr ((q,c), (p, m)) ->
                  let account2 = Option.defaultValue account contraAccount
                  [account, p, m; conversionsAccount, -p, m
                   account2, -q, c; conversionsAccount, q, c]
              | Cl ((q,c), (p, m)) ->
                  let account2 = Option.defaultValue account contraAccount
                  [account, q, c; conversionsAccount, -q, c
                   account2, -p, m; conversionsAccount, p, m]

              // match settAccount with
              //   | None -> [account, value]
              //   | Some a -> match value with
              //                 | None -> raise (ArgumentException "Must provide a value when using settlement account")
              //                 | Some u -> [account, value ; a, Some <| contraRAmountV u]
          let ys = nonBlanks |> List.collect createContraV
          Some <| Choice1Of2 ys

      with
        | :? ArgumentException as ae -> Some (Choice2Of2 ae.Message)


  | elt -> None

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

  let xyz = elts |> List.choose (balanceEntry globalDefaultCommodity accountDecls commodityDecls)

  printfn "%A" xyz
  elts