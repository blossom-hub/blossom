module Renderers

open Tabular
open Types

module HumanReadable =
  let seqToLines xs =
    for x in xs do
      printfn "%s" x

  let renderTable = renderText

  let renderMetaResult =
    function | Statistics s    -> printfn "%A" s
             | MetaResultSet s -> seqToLines s
