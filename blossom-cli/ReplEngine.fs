module ReplEngine

open System

open FParsec

open Types
open ReplParser


let execute state input =
  let action = function
    | Quit          -> None
    | Clear         -> Console.Clear()
                       Some state
    | Load filename -> Some state
    | Reload        -> Some state
    | Balances s    -> Some state

  match run parse input with
    | Success (result, _, _) ->
        printfn "=> %A" result
        action result
    | Failure (errorMessage, _, _) ->
        printfn "=> error detected %A" errorMessage
        Some state

let rec go state =
  printf "] "
  let input = Console.ReadLine()
  match execute state input with
    | Some state2 -> go state2
    | None        -> ()