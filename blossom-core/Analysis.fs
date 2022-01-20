module Analysis

open System
open Types
open Shared
open JournalParser

open FParsec

type 'a H = Position * SQ * bool * 'a

type private OCM = {
  Opening: DTrade
  Closing: DTrade
  OSeq: SQ * int
  CSeq: SQ * int
  Quantity: decimal
}

let lookupK kfactors asset dt =
  kfactors |> Map.tryFind asset
           |> Option.map (fun xs -> xs |> List.filter (fun ks -> fst ks >= dt) |> List.head |> snd)
           |> Option.defaultValue 1M

let multiplierOf commodityDecls c =
  commodityDecls |> Map.tryFind c
                 |> Option.bind (fun d -> d.Multiplier)
                 |> Option.defaultValue 1m

let isMtm commodityDecls c =
  commodityDecls |> Map.tryFind c
                 |> Option.bind (fun d -> Some d.Mtm)
                 |> Option.defaultValue false

let navIndicatorOf commodityDecls c = isMtm commodityDecls c |> function true -> 1M | false -> 0M

let analyseInvestments commodityDecls
                       (trades : DTrade H list)
                       (dividends : DDividend H list)
                       (prices : Map<(Commodity*Commodity), Map<DateTime, decimal>>)
                       (kfactors : Map<Commodity, (DateTime * decimal) list>) =

  // Step 1a: Calculate the split adjustment factors, update the trades
  let ktrades = trades |> List.map (fun (posn, sq, flag, dtrade) -> let k = lookupK kfactors dtrade.Asset (fst sq)
                                                                    let ktrade = {dtrade with Keff = k
                                                                                              Quantity = k * dtrade.Quantity
                                                                                              PerUnitPrice = first (fun p -> p/k) dtrade.PerUnitPrice}
                                                                    (posn, sq, flag, ktrade))

  // Step 1b: Update prices with factors too; also augment prices from trades if a price is missing for a day that was traded (e.g. fill in some blanks)
  let prices1 = trades |> List.fold (fun s (_, sq, _, dtrade) -> let key = (dtrade.Asset, snd dtrade.PerUnitPrice)
                                                                 let history = s |> Map.tryFind key |> Option.defaultValue Map.empty
                                                                 let price = history |> Map.tryFind (fst sq)
                                                                 match price with
                                                                   | None -> history |> Map.add (fst sq) (fst dtrade.PerUnitPrice)
                                                                                     |> fun hs -> Map.add key hs s
                                                                   | Some _ -> s
                                                                 ) prices

  let kprices = prices1 |> Map.map (fun k ts -> ts |> Map.map (fun dt p -> let k = lookupK kfactors (fst k) dt in (p/k, k)))

  // Step 2: Tag each trade event
  let f1 balances (sq, dtrade: DTrade) =
    let key = (dtrade.Account, dtrade.Asset)
    let currentBalance = balances |> Map.tryFind key
    match currentBalance with
      | None         -> (Open, sq, dtrade), balances |> Map.add key dtrade.Quantity
      | Some balance -> let currentlyLong = balance > 0M
                        let tradeLong = dtrade.Quantity > 0M
                        let newBalance = balance + dtrade.Quantity
                        let lt = match newBalance, currentlyLong, tradeLong with
                                   | 0M, _, _        -> Close
                                   | _, true, true   -> Extend // (Long)
                                   | _, false, false -> Extend // (Short)
                                   | _, _, _         -> Reduce
                        let newBalances = match newBalance with
                                            | 0M -> Map.remove key balances
                                            | b  -> Map.add key b balances
                        (lt, sq, dtrade), newBalances

  let taggedTrades, _balances = ktrades |> List.map (snd4 &&& frh4) |> List.mapFold f1 Map.empty

  // Step 3: Match up open/close style lots.
  // This currently uses FIFO but later will respect the lotnames provided in the document
  // to provide directed PnL calculations.
  let openTs, closeTs = taggedTrades |> List.mapi (fun i (lt, sq, tr) -> (lt, (sq, i), tr.Quantity, tr))
                                     |> List.partition (fun (lt, _, _, _) -> match lt with | Open | Extend -> true | _ -> false)
                                     |> first (List.map (fun (lt, sqi, trq, tr) -> (lt, sqi, trq, tr, List.empty)))

  let f2 ots ct =
    let f3 cti otj =
      let clt, csqi, cqty, (ctr: DTrade) = cti
      let olt, osqi, oqty, (otr: DTrade), closures = otj
      if (csqi > osqi) && (ctr.Asset = otr.Asset) && (ctr.Account = otr.Account) && (cqty <> 0M) && (oqty <> 0M)
        then
          let amt = min (abs cqty) (abs oqty)
          let cq = decimal(sign cqty) * amt
          let oq = decimal(sign oqty) * amt
          let ocm = {Opening = otr; Closing = ctr; OSeq = osqi; CSeq = csqi; Quantity = cq}
          let closures2 = closures @ [ocm]
          let cti2 = (clt, csqi, cqty - cq, ctr)
          let otj2 = (olt, osqi, oqty - oq, otr, closures2)
          otj2, cti2
        else otj, cti
    let newPool, _ = List.mapFold f3 ct ots
    1, newPool

  let matched = List.mapFold f2 openTs closeTs |> snd |> List.collect fih5

  // Step 4: Calculate gains per match
  let capitalGains =
    flip List.map matched <| fun ocm -> let pm = fst ocm.Closing.PerUnitPrice - fst ocm.Opening.PerUnitPrice
                                        let multiplier = multiplierOf commodityDecls ocm.Opening.Asset
                                        let pnl = pm * Math.Abs(ocm.Quantity) * multiplier * decimal(Math.Sign(ocm.Opening.Quantity))
                                        ocm, pnl

  // Step 5: Build report with matching, pnl etc.
  let matched2 =
    openTs |> List.map (
      fun (_lt, sqi, _q, dtrade, _) ->
        let closings =
          capitalGains |>
            List.choose (fun (ocm, pnl) ->
              if (ocm.Opening = dtrade) && (ocm.OSeq = sqi)
                then
                  let closingSettlement = Option.defaultValue ocm.Closing.Account ocm.Closing.Settlement
                  Some {
                    ClosingTrade.Date = fst ocm.CSeq
                    Settlement = closingSettlement
                    CapitalGains = ocm.Closing.CapitalGains
                    Quantity = ocm.Quantity
                    Keff = ocm.Closing.Keff
                    PerUnitPrice = fst ocm.Closing.PerUnitPrice
                    UnadjustedPnL = pnl
                    LotName = ""
                    Reference = ocm.Closing.Reference
                    Expenses =  ocm.Closing.Expenses |> List.map (fun (a, v, c) -> (a, v, match c with | Some CS -> a | Some (CV a2) -> a2 | None -> closingSettlement))
                  }
                else None)
        let openingSettlement = Option.defaultValue dtrade.Account dtrade.Settlement
        let totalClosingQuantity = closings |> List.sumBy (fun c -> c.Quantity)
        let lastPrice = kprices |> Map.find (dtrade.Asset, snd dtrade.PerUnitPrice)
                                |> Map.toList |> List.maxBy fst |> second fst
        let openQuantity = dtrade.Quantity + totalClosingQuantity
        let unrealisedPnL = openQuantity * (snd lastPrice - fst dtrade.PerUnitPrice) * multiplierOf commodityDecls dtrade.Asset
        {
          OpeningTrade.Date = fst sqi
          Account = dtrade.Account
          Settlement = openingSettlement
          Asset = dtrade.Asset
          Quantity = dtrade.Quantity
          Keff = dtrade.Keff
          OpenQuantity = openQuantity
          Measure = snd dtrade.PerUnitPrice
          PerUnitPrice = fst dtrade.PerUnitPrice
          LastUnitPrice = lastPrice
          UnrealisedPnL = unrealisedPnL
          LotName = ""
          Reference = dtrade.Reference
          Expenses = dtrade.Expenses |> List.map (fun (a, v, c) -> (a, v, match c with | Some CS -> a | Some (CV a2) -> a2 | None -> openingSettlement))
          Closings = closings
        })
  matched2, kprices
