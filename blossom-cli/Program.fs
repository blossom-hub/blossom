﻿open System
open PrettyPrint

let VERSION = "0.52"
let VERSION_STRING = $"Blossom {VERSION}"
[<EntryPoint>]
let main argv =
  let cmds = ["http"; "pp"]
  match Array.toList argv with
    | (fn::[]) when not (List.contains fn cmds)
           -> printfn $"{VERSION_STRING}"
              ReplEngine.repl1 fn
    | []   -> printfn $"{VERSION_STRING}"
              ReplEngine.repl ReplEngine.State.Default
    | (cmd::args) -> match cmd.ToLower() with
                      | "http" -> match args with
                                    | [p] -> let port = int p
                                             WebEngine.web port
                                    | _ -> failwith "Expected 1 port argument for web server startup"
                      | "pp"   -> match args with
                                    | [ifn; it; ot; ofn] -> PrettyPrint.pp (parseFileFormat it) (parseFileFormat ot) ifn ofn
                                    | _ -> failwith "Expected 4 arguments for pretty printer"
                      | _ -> failwith $"Unrecognised command: {cmd}"
  0