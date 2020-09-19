module Renderers

module HumanReadable =
  let seqToLines xs =
    for x in xs do
      printfn "%s" x