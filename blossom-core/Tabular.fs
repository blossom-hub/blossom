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
  | Empty

type Table = Table of columns:ColumnSpec list * rows:ColumnData list list

let isElidable = function | Number _ -> false
                          | _ -> true

let rec initialRender =
  function | Date d         -> d.ToString("yyyy-MM-dd")
           | Text s         -> s
           | Number (v, dp) -> v.ToString("0." + String.replicate dp "0" + "##")
           | Empty          -> ""

let renderText1 maxRows (Table (hs, data)) =
  let elidables0 = data |> List.tryHead |> Option.map (List.map isElidable)
  let rows = data |> List.map (List.map initialRender)
  let getWidth i (xs : string list list) = xs |> List.map (fun r -> r.[i] |> String.length) |> List.max

  let count = List.length rows
  let rows1, extraRows = if maxRows = 0 || count <= maxRows 
                           then rows, None 
                           else rows.[0..maxRows-1], Some (List.length rows - maxRows)

  match rows1 with
    | [] -> []
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
            let printed, _ = rs |> List.mapFold f initialLast

            let combined = [headers] @ printed
            let lines1 = combined |> List.map (String.concat "   ")
            let lines2  = match extraRows with
                            | Some n -> [$"  (first {List.length rs} rows, {n} rows hidden)"]
                            | None   -> [sprintf "  (%A rows)" (List.length rs)]
            lines1 @ lines2

let renderText = renderText1 0

// Since we have close control over the object, render this directly rather than
// using Newtonsoft.Json
let renderJson (Table (hs, data)) = 
  let L x = "[" + x + "]"
  let Q x = "\"" + x + "\""
  let schema = hs |> List.map (fun cs -> $"""{{"header":{Q cs.Header}, "type":{if cs.Key then "true" else "false"}}}""")
                  |> String.concat ","
                  |> L

  let render e = let i = initialRender e
                 match e with | Date _ -> $"""{{"d": {Q i}}}"""
                              | Text _ -> Q i
                              | Empty  -> "null"
                              | _      -> i

  let rows = data |> List.map (List.map render >> String.concat "," >> L) |> String.concat "," |> L

  $"""{{"schema": {schema}, "rows": {rows}}}"""


