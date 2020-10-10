module ReplEngine

open System
open System.IO

open Types
open Renderers
open ReplParser
open ParserShared
open SubParsers
open Journal

open Reports

type State =
  {
    Filename: string option
    Journal: Journal option
  }
  with
    static member Default = {
      Filename = None
      Journal = None
    }

let getFilter = FromString >> runParser pFilter JournalParser.UserState.Default

let load state filename =
  try
    let parsed = loadJournal filename
    Some {state with Journal = Some parsed; Filename = Some filename}
  with
    | :? FileNotFoundException as ex ->
           printfn "Couldn't find file %A" filename
           Some state

let reload state =
  match state.Filename with
    | Some fn -> load state fn
    | None    -> printfn "Cannot reload if no file loaded"
                 Some state

let execute state input =
  let withJournal op =
    match state.Journal with
      | Some j -> op j
      | None   -> printfn "You must load a journal first."
    Some state

  let action = function
    | Quit                 -> None
    | Clear                -> Console.Clear()
                              Some state
    | Load filename        -> load state filename
    | Reload               -> reload state
    | Balances query       -> withJournal <| balances HumanReadable.renderTable (getFilter query)
    | Journal query        -> withJournal <| journal HumanReadable.renderTable (getFilter query)
    | Meta request         -> withJournal <| meta HumanReadable.seqToLines request

  try
    let result = runParser parse () (FromString input)
    printfn "=> %A" result
    action result
  with
    | ex -> printfn "=> error detected %A" ex.Message
            Some state

let rec go state =
  printf "] "
  let input = Console.ReadLine()
  match execute state input with
    | Some state2 -> go state2
    | None        -> ()