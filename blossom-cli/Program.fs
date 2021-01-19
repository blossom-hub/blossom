// Learn more about F# at http://fsharp.org

open System
open Argu

type OperatingMode = Interactive | LSP | Http

type Arguments =
  | [<Unique>] Interactive of file:string
  | [<Unique>] Http of file:string
  | [<Unique>] [<Hidden>] LSP
  | Port of int
  with
    interface IArgParserTemplate with
      member s.Usage =
        match s with
          | Interactive _ -> "launch interactive console for <file>."
          | Http (_) -> $"start in-process webserver for <file>, on <port> (default: {WebEngine.defaultPort})."
          | LSP -> "start persistent executable for language server."
          | Port _ -> "override <port> for webserver"

[<EntryPoint>]
let main argv =
    printfn "Blossom 0.1"
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.Parse argv

    // prefer interactive over http
    let isInteractive = results.TryGetResult Interactive
    let isHttp = results.TryGetResult Http
    let isLSP = results.Contains LSP

    if isLSP
      then LspEngine.lsp ()
      else match isInteractive with
            | Some fn -> ReplEngine.repl1 fn
            | None -> match isHttp with
                        | Some fn -> let port = results.TryGetResult Port
                                     WebEngine.web fn port
                        | None -> ReplEngine.repl ReplEngine.State.Default

    0
