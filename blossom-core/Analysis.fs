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
  OSeq: SQ
  CSeq: SQ
  Quantity: decimal
}

let analyseInvestments (trades : DTrade H list) (dividends : DDividend H list)
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
  let openTs, closeTs = taggedTrades |> List.partition (fun (lt, _, _) -> match lt with | Open | Extend -> true | _ -> false)
                                     |> first (List.map (fun (lt, sq, tr) -> (lt, sq, tr.Quantity, tr, List.empty)))
                                     |> second (List.map (fun (lt, sq, tr) -> (lt, sq, tr.Quantity, tr)))

  // FIXME need SQ rather than DT here, and then to _really_ create an intra-day ordering
  let f2 ots ct =
    let f3 cti otj =
      let clt, csq, cqty, (ctr: DTrade) = cti
      let olt, osq, oqty, (otr: DTrade), closures = otj
      if (csq > osq) && (ctr.Asset = otr.Asset) && (ctr.Account = otr.Account) && (cqty <> 0M)
        then let amt = min (abs cqty) (abs oqty)
             let cq = decimal(sign cqty) * amt
             let oq = decimal(sign oqty) * amt
             let ocm = {Opening = otr; Closing = ctr; OSeq = osq; CSeq = csq; Quantity = Math.Abs(amt)}
             let closures2 = closures @ [ocm]
             let cti2 = (clt, csq, cqty - cq, ctr)
             let otj2 = (olt, osq, oqty - oq, otr, closures2)
             otj2, cti2
        else otj, cti
    let newPool, _ = List.mapFold f3 ct ots
    1, newPool

  let matched = List.mapFold f2 openTs closeTs |> snd |> List.collect fih5

  // Step 3: Calculate gains per match
  let capitalGains =
    flip List.map matched <| fun ocm -> let pm = fst ocm.Closing.PerUnitPrice - fst ocm.Opening.PerUnitPrice
                                        let multiplier = 1M // FIXME!
                                        let pnl = pm * ocm.Quantity * multiplier * decimal(Math.Sign(ocm.Opening.Quantity))
                                        ocm, pnl

  // Step 4: Build report with matching, pnl etc.
  let matched2 =
    openTs |> List.map (
      fun (_lt, sq, _q, dtrade, _) ->
        let closings =
          capitalGains |>
            List.choose (fun (ocm, pnl) ->
              if (ocm.Opening = dtrade) && (ocm.OSeq = sq)
                then
                  let closingSettlement = Option.defaultValue ocm.Closing.Account ocm.Closing.Settlement
                  Some {
                    ClosingTrade.Date = ocm.CSeq
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
          OpeningTrade.Date = sq
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
