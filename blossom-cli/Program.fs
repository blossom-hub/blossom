open Argu
open System
open PrettyPrint

type OperatingMode = Interactive | LSP | Http | PP

type HttpArgs =
 | [<MainCommand; ExactlyOnce; Last>] File of journal:string
 | Port of port:int
 with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
        | File _ -> "Journal file for processing"
        | Port _ -> $"Port for webservice, default: {WebEngine.defaultPort}."
and PPArgs =
  | [<ExactlyOnce>] InputFile of journal:string
  | [<ExactlyOnce>] OutputFile of string
  | [<ExactlyOnce>] InputType of FileFormat
  | [<ExactlyOnce>] OutputType of FileFormat
    with
      interface IArgParserTemplate with
        member s.Usage =
          match s with
            | InputFile _  -> $"Input file for pretty printing."
            | OutputFile _ -> $"Output file for pretty printing."
            | InputType _  -> $"Type of input file."
            | OutputType _ -> $"Type to use for output file."
and LSPArgs =
  | Dummy
    with
      interface IArgParserTemplate with
        member s.Usage =
          match s with
            | Dummy -> "Test flag"
and Arguments =
  | [<CliPrefix(CliPrefix.None)>] Http of ParseResults<HttpArgs>
  | [<CliPrefix(CliPrefix.None)>] PP of ParseResults<PPArgs>
  | [<CliPrefix(CliPrefix.None)>] [<Hidden>] LSP of ParseResults<LSPArgs>
  | [<MainCommand; Last>] File of journal:string
  with
   interface IArgParserTemplate with
     member s.Usage =
       match s with
         | Http _ -> "Start the http server."
         | PP _   -> "Transcode and pretty print files."
         | LSP _   -> "Start the Language Service for VSCode etc."
         | File _   -> "Provide a file to start up interactive mode."


[<EntryPoint>]
let main argv =
    printfn "Blossom 0.1"
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.Parse argv

    let isHttp = results.Contains Http
    let isLSP = results.Contains LSP
    let isPP = results.Contains PP

    let input = results.TryGetResult File

    match true with
      | _ when isLSP  -> LspEngine.lsp()
      | _ when isHttp -> let httpargs = results.GetResult Http
                         let port = httpargs.GetResult Port
                         WebEngine.web input port
      | _ when isPP   -> let ppargs = results.GetResult PP
                         let it = ppargs.GetResult InputType
                         let ot = ppargs.GetResult OutputType
                         let ofn = ppargs.GetResult OutputFile
                         PrettyPrint.pp it ot (Option.get input) ofn
      | _ -> match input with
               | Some fn -> ReplEngine.repl1 fn
               | None    -> ReplEngine.repl ReplEngine.State.Default
    0
