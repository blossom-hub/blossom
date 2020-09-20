module Renderers

open Tabular

module HumanReadable =
  let seqToLines xs =
    for x in xs do
      printfn "%s" x

  let renderTable = renderText