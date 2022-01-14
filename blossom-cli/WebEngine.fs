module WebEngine

open System
open System.Collections.Generic

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Utils.Collections

open ReplParser
open ParserShared
open Types
open Journal
open Reports
open Renderers

let STATELOCK = Object()

type State = {
  Journals: Dictionary<int, string * Journal>
}
  with
    static member Default = {
      Journals = new Dictionary<int, string * Journal>()
    }
    member this.nextId () = 
      lock STATELOCK (fun () -> let keys = this.Journals.Keys
                                if Seq.isEmpty keys then 0 else Seq.max keys |> fun k -> k + 1)
    member this.getFilename index = this.Journals.TryGetValue index |> function | true, (fn, _) -> Some fn | _ -> None
    member this.getIndex filename = this.Journals |> Seq.tryPick (fun (KeyValue(k, (fn, _))) -> if fn = filename then Some k else None)
    member this.getJournal index = this.Journals |> Seq.tryPick (fun (KeyValue(i, (_, j))) -> if i = index then Some j else None)
    member this.getJournal filename = this.Journals |> Seq.tryPick (fun (KeyValue(_, (fn, j))) -> if fn = filename then Some j else None)

let load (state: State) filename =
  lock STATELOCK (fun () -> 
    let usedId = match state.getIndex filename with 
                  | None   -> state.nextId()
                  | Some i -> i
    let journal = loadJournal false DateTime.Now filename
    state.Journals.[usedId] <- (filename, journal)
    usedId
 )

let execute (state: State) (index: int, input) =
  let result = runParser parse () (FromString input)

  let withJournal op = 
    let journal = state.getJournal index
    match journal with 
      | Some j -> op j |> Choice1Of2
      | None   -> Choice2Of2 "Invalid journal id"

  match result with 
    | Balances (filter, request)         -> withJournal <| balances Tabular.renderJson filter request
    | Journal (filter, request)          -> withJournal <| journal Tabular.renderJson filter request
    | BalanceSeries (filter, request)    -> withJournal <| balanceSeries Tabular.renderJson filter request
    | LotAnalysis (filter, request)      -> withJournal <| lotAnalysis Tabular.renderJson filter request
    | HoldingsAnalysis filter            -> withJournal <| holdingsAnalysis Tabular.renderJson filter true
    | Check request                      -> withJournal <| checkJournal Tabular.renderJson request
    //| Meta request                       -> withJournal <| meta HumanReadable.renderMetaResult request
    | _ -> Choice2Of2 "Unsupported via http!"

let web port =
  let config = { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" port] }

  let state = State.Default

  startWebServer config <| 
    choose [
      GET >=> choose [
        path "/rest" >=> OK "In peace" 
        pathScan "/rest/load/%s" (load state >> string >> OK)
        pathScan "/rest/%d/filename" (fun i -> state.getFilename i |> Option.defaultValue "" |> OK)
      ]
      POST >=> choose [
        pathScan "/rest/%d/execute" (fun i -> request (fun r -> let command = fst r.form.[0]
                                                                let res = execute state (i, command) 
                                                                match res with 
                                                                  | Choice1Of2 a -> OK a
                                                                  | Choice2Of2 b -> failwith b))
      ]
      OK "Hi there"
    ]