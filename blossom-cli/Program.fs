open Argu
open System
open PrettyPrint

type OperatingMode = Interactive | Http | PP

type HttpArgs =
 | Port of port:int
 with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
        | Port _ -> "Port for webservice"
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
and Arguments =
  | [<CliPrefix(CliPrefix.None)>] Http of ParseResults<HttpArgs>
  | [<CliPrefix(CliPrefix.None)>] PP of ParseResults<PPArgs>
  | [<MainCommand; Last>] File of journal:string
  with
   interface IArgParserTemplate with
     member s.Usage =
       match s with
         | Http _ -> "Start the http server."
         | PP _   -> "Transcode and pretty print files."
         | File _ -> "Provide a file to start up interactive mode."


[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.Parse argv

    let isHttp = results.Contains Http
    let isPP = results.Contains PP

    let input = results.TryGetResult File

    match true with
      | _ when isHttp -> let httpargs = results.GetResult Http
                         let port = httpargs.GetResult Port
                         WebEngine.web port
      | _ when isPP   -> let ppargs = results.GetResult PP
                         let ifn = ppargs.GetResult InputFile
                         let it = ppargs.GetResult InputType
                         let ot = ppargs.GetResult OutputType
                         let ofn = ppargs.GetResult OutputFile
                         PrettyPrint.pp it ot ifn ofn
      | _ -> printfn "Blossom 0.5"
             match input with
               | Some fn -> ReplEngine.repl1 fn
               | None    -> ReplEngine.repl ReplEngine.State.Default
    0
