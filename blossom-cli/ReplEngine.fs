module ReplEngine

open System
open System.IO

open Types
open ReplParser
open ParserShared
open JournalParser

type State =
  {
    Filename: string option
    Journal: RJournal option
  }
  with
    static member Default = {
      Filename = None
      Journal = None
    }

let load state filename =
  try
    let parsed = loadRJournal filename
    printfn "%A" parsed
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
  let action = function
    | Quit          -> None
    | Clear         -> Console.Clear()
                       Some state
    | Load filename -> load state filename
    | Reload        -> reload state
    | Balances s    -> Some state

  try
    let result = runParser parse () (FromString input)
    printfn "=> %A" result
    action result
  with
    | :? InvalidOperationException as ex ->
          printfn "=> error detected %A" ex.Message
          Some state

let rec go state =
  printf "] "
  let input = Console.ReadLine()
  match execute state input with
    | Some state2 -> go state2
    | None        -> ()