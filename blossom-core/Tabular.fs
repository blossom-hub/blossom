module Tabular

open System
open Shared

type ColumnSpec =
  { Header: string
    Key: bool }

type ColumnData =
  | Date of DateTime
  | Text of string
  | Number of decimal * int
  | List of ColumnData list
  | Empty

type Table = Table of columns:ColumnSpec list * rows:ColumnData list list

let isElidable = function | Number _ | List _ -> false
                          | _ -> true

let rec initialRender =
  function | Date d        -> d.ToString("yyyy-MM-dd")
           | Text s        -> s
           | Number (v,dp) -> Decimal.Round(v, dp) |> sprintf "%M"
           | Empty         -> ""
           | List xs       -> xs |> List.map initialRender |> String.concat ", "

let renderText (Table (hs,data)) =
  let elidables0 = data |> List.tryHead |> Option.map (List.map isElidable)
  let rows = data |> List.map (List.map initialRender)
  let getWidth i (xs : string list list) = xs |> List.map (fun r -> r.[i] |> String.length) |> List.max

  match rows with
    | [] -> ()
    | rs -> let widths = hs |> List.mapi (fun i h -> max (String.length h.Header) (getWidth i rs))

            // elidables will be a Some
            let elidables = Option.get elidables0

            // headers
            let headers = List.map2 (fun w h -> sprintf "%*s" w h.Header) widths hs

            // data
            let initialLast = List.replicate (List.length headers) (uid())
            let f last row =
              // if any key columns change, print the whole row
              let fresh = List.map3 (fun p t s -> match s.Key with true -> Some (p <> t) | _ -> None) last row hs
                            |> List.choose id
                            |> List.exists id
              let row' = match fresh with
                           | true  -> row
                           | false -> List.map3 (fun p r -> function | false -> r
                                                                     | true  -> if p = r then "" else r) last row elidables
              let result = List.map2 (fun w d -> sprintf "%*s" w d) widths row'
              result, row
            let printed, _ = rows |> List.mapFold f initialLast

            let combined = [headers] @ printed
            combined |> List.map (String.concat "   ") |> String.concat Environment.NewLine |> printfn "%s"
            printfn "  (%A rows)" (List.length rs)


