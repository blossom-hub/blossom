module ReplEngine

open System
open System.IO

open Types
open Renderers
open ReplParser
open ParserShared
open Journal
open Tabular
open Shared

open Reports

type GlobalOptions =
  {
    PerformanceReporting: bool
    Debug: bool
    LoadTracing: bool
    ValuationDate: DateTime option
    MaxRows: int
  }
  with
    static member Default = {
      PerformanceReporting = false
      Debug = false
      LoadTracing = false
      ValuationDate = None
      MaxRows = 1000
    }

type State =
  {
    ActiveJournal: int option
    Journals : Map<int, string * DateTime * Journal>
    GlobalOptions: GlobalOptions
  }
  with
    static member Default = {
      ActiveJournal = None
      Journals = Map.empty
      GlobalOptions = GlobalOptions.Default
    }

let time action =
  let timer = Diagnostics.Stopwatch.StartNew()
  let result = action()
  let elapsed = timer.Elapsed
  result, elapsed

let load state filename =
  try
    let parsed = loadJournal state.GlobalOptions.LoadTracing state.GlobalOptions.ValuationDate filename
    let existingId = Map.tryPick (fun k (fn, _, _) -> if fn = filename then Some k else None) state.Journals
    let usedId = match existingId with
                   | Some i -> i
                   | None -> state.Journals |> Map.toList |> List.map fst
                                                          |> function | [] -> 1 | xs -> List.max xs |> fun k -> k + 1
    Some {state with ActiveJournal = Some usedId; Journals = state.Journals |> Map.add usedId (filename, DateTime.Now, parsed)}
  with
    | :? FileNotFoundException as ex ->
           printfn "Couldn't find file %A" ex.FileName
           Some state

let close state journalId =
  if state.Journals.ContainsKey journalId
    then let newJournals = state.Journals |> Map.remove journalId
         let newActiveJournal =
           if state.ActiveJournal <> Some journalId
             then state.ActiveJournal
             else newJournals |> Map.toList |> function | [] -> None | xs -> List.minBy fst xs |> fst |> Some
         printfn "Closed: %s" (state.Journals.[journalId] |> fst3)
         Some { state with Journals = newJournals; ActiveJournal = newActiveJournal }
    else printfn "Journal identifier not valid"
         Some state

let reload state =
  match state.ActiveJournal with
    | Some i -> state.Journals.[i] |> fst3 |> load state
    | None   -> printfn "Cannot reload if no file loaded"
                Some state

let switch state journalId =
  match journalId with
    | None   -> let cs = [{Header = "#"; Key = true}
                          {Header = "A"; Key = false;}
                          {Header = "Filename"; Key = false}
                          {Header = "Load time"; Key = false}]
                state.Journals |> Map.toList
                               |> List.map (fun (k, (fn, t, _)) -> [Number (decimal k,0)
                                                                    Text (if Some k = state.ActiveJournal then "*" else "")
                                                                    Text fn
                                                                    Text (t.ToString())])
                               |> fun ds -> Table (cs, ds)
                               |> HumanReadable.renderTable
                               |> ignore
                Some state
    | Some j -> match Map.tryFind j state.Journals with
                  | Some _ -> let fn = state.Journals.[j] |> fst3
                              printfn "Switched to: %s" fn
                              Some {state with State.ActiveJournal = Some j}
                  | None   -> printfn "Journal identifier not valid"
                              Some state

let set state value =
  match value with
   | None -> printfn "%A" state.GlobalOptions; Some state
   | Some (GPerformanceReporting v)
       -> Some {state with GlobalOptions = {state.GlobalOptions with PerformanceReporting = v}}
   | Some (GDebug v)
       -> Some {state with GlobalOptions = {state.GlobalOptions with Debug = v}}
   | Some (GLoadTracing v)
       -> Some {state with GlobalOptions = {state.GlobalOptions with LoadTracing = v}}
   | Some (GValuationDate v)
       -> reload {state with GlobalOptions = {state.GlobalOptions with ValuationDate = v}}
   | Some (GMaxRows v)
       -> Some {state with GlobalOptions = {state.GlobalOptions with MaxRows = v}}

let showHelp state =
  printfn "  Filters"
  printfn "    date: >,>=,<,<= or =="
  printfn "    payee: @"
  printfn "    narrative: ?"
  printfn "    commodity: %%"
  printfn "    measure: %%%%"
  printfn "    tag: +"
  printfn "    virtual account: /"
  printfn "    account: no symbol"
  Some state

let execute state input =
  let withJournal op =
    match state.ActiveJournal with
      | Some i -> state.Journals.[i] |> thd3 |> op |> ignore
      | None   -> printfn "You must load a journal first."
    Some state

  let action = function
    | Quit                               -> None
    | Clear                              -> Console.Clear()
                                            Some state
    | Set value                          -> set state value
    | Load filename                      -> load state filename
    | Close journalId                    -> close state journalId
    | Switch journalId                   -> switch state journalId
    | Reload                             -> reload state
    | Balances (filter, request)         -> withJournal <| balances HumanReadable.renderTable filter request
    | Journal (filter, request)          -> withJournal <| journal (HumanReadable.renderTable1 state.GlobalOptions.MaxRows) filter request
    | BalanceSeries (filter, request)    -> withJournal <| balanceSeries HumanReadable.renderTable filter request
    | LotAnalysis (filter, request)      -> withJournal <| lotAnalysis HumanReadable.renderTable filter request
    | HoldingsAnalysis filter            -> withJournal <| holdingsAnalysis HumanReadable.renderTable filter true
    | Check request                      -> withJournal <| checkJournal HumanReadable.renderTable request
    | Meta request                       -> withJournal <| meta HumanReadable.renderMetaResult request
    | Help                               -> showHelp state

  try
    let result = runParser parse () (FromString input)
    if state.GlobalOptions.Debug
      then printfn "=> %A" result
      else ()
    let output, duration = time (fun () -> action result)
    match state.GlobalOptions.PerformanceReporting with
      | true -> printfn "=> %A elapsed." duration
      | false -> ()
    output
  with
    | ex -> printfn "=> error detected %A" ex.Message
            Some state

let rec repl state =
  match state.ActiveJournal with
    | Some i -> printf "%d] " i
    | None   -> printf "] "
  let input = Console.ReadLine()
  match execute state input with
    | Some state2 -> repl state2
    | None        -> ()

let rec repl1 filename =
  let state = load State.Default filename
  match state with
    | Some s -> repl s
    | None -> failwith "Unexpected load error"