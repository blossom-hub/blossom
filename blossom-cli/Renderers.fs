module Renderers

open Tabular
open Types

module HumanReadable =
  let seqToLines xs =
    for x in xs do
      printfn "%s" x
    xs

  let renderTable = renderText >> seqToLines

  let renderMetaResult =
    function | Statistics s    -> [sprintf "%A" s] |> seqToLines
             | MetaResultSet s -> Set.toList s |> seqToLines
