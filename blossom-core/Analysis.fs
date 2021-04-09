module Analysis

open System
open Types
open Shared
open Definitions
open JournalParser

open FParsec

type 'a H = Position * SQ * bool * 'a

type DividendPayment = {
  ExDate: DateTime
  PayDate: DateTime
  Account: Account
  SettlementAccount: Account
  IncomeAccount: Account
}

type private OCM = {
  Opening: DTrade
  Closing: DTrade
  OSeq: SQ * int
  CSeq: SQ * int
  Quantity: decimal
}

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
                       (trades : DTrade H list) (dividends : DDividend H list)
                       (prices : DPrice H list) (splits : DSplit H list) =

  // Step 1: Tag each trade event
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
                        (lt, sq, dtrade), balances |> Map.add key newBalance

  let taggedTrades, _balances = trades |> List.map (snd4 &&& frh4) |> List.mapFold f1 Map.empty

  // Step 2: Match up open/close style lots.
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

  // Step 3: Calculate gains per match
  let capitalGains =
    flip List.map matched <| fun ocm -> let pm = fst ocm.Closing.PerUnitPrice - fst ocm.Opening.PerUnitPrice
                                        let multiplier = multiplierOf commodityDecls ocm.Opening.Asset
                                        let pnl = pm * Math.Abs(ocm.Quantity) * multiplier * decimal(Math.Sign(ocm.Opening.Quantity))
                                        ocm, pnl

  // Step 4: Build report with matching, pnl etc.
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
                    PerUnitPrice = ocm.Closing.PerUnitPrice
                    UnadjustedPnL = pnl, snd ocm.Closing.PerUnitPrice
                    LotName = ""
                    Reference = ocm.Closing.Reference
                    Expenses =  ocm.Closing.Expenses |> List.map (fun (a, v, c) -> (a, V v, match c with | Some CS -> a | Some (CV a2) -> a2 | None -> closingSettlement))
                  }
                else None)
        let openingSettlement = Option.defaultValue dtrade.Account dtrade.Settlement
        {
          OpeningTrade.Date = fst sqi
          Account = dtrade.Account
          Settlement = openingSettlement
          Asset = dtrade.Asset
          Quantity = dtrade.Quantity
          PerUnitPrice = dtrade.PerUnitPrice
          LotName = ""
          Reference = dtrade.Reference
          Expenses = dtrade.Expenses |> List.map (fun (a, v, c) -> (a, V v, match c with | Some CS -> a | Some (CV a2) -> a2 | None -> openingSettlement))
          Closings = closings
        })
  matched2
