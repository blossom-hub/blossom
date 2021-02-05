open Argu
open System

type OperatingMode = Interactive | LSP | Http | PP

type Arguments =
  | [<Unique>] Interactive of file:string
  | [<Unique>] Http of file:string * port:int
  //| [<Unique>] PP of inputType:FileFormat * outputType:FileFormat * input:string * output:string
  | [<Unique>] [<Hidden>] LSP
  with
    interface IArgParserTemplate with
      member s.Usage =
        match s with
          | Interactive _ -> "launch interactive console for <file>."
          | Http (_) -> $"start in-process webserver for <file>, on <port> (default: {WebEngine.defaultPort})."
//        | PP _ -> "run pretty printing engine / transcoder."
          | LSP -> "start persistent executable for language server."

[<EntryPoint>]
let main argv =
    printfn "Blossom 0.1"
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.Parse argv

    // prefer interactive over http
    let isInteractive = results.Contains Interactive
    let isHttp = results.Contains Http
    let isLSP = results.Contains LSP
//  let isPP = results.Contains PP

    match true with
      | _ when isLSP  -> LspEngine.lsp()
      | _ when isHttp -> let fn, port = results.GetResult Http
                         WebEngine.web fn port
//    | _ when isPP   -> let it, ot, ifn, ofn = results.GetResult PP
//                       PrettyPrint.pp it ot ifn ofn
      | _ when isInteractive -> let fn = results.GetResult Interactive
                                ReplEngine.repl1 fn
      | _ -> ReplEngine.repl ReplEngine.State.Default

    0
