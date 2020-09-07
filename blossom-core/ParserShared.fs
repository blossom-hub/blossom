module ParserShared

open System
open FParsec

type Input =
  | FromFile of string
  | FromString of string

let runParser p s0 input =
  let encoding = System.Text.Encoding.UTF8
  let res =
    match input with
      | FromFile filename -> runParserOnFile p s0 filename encoding
      | FromString direct -> runParserOnString p s0 "" direct
  match res with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _parserError, _userState) -> raise (InvalidOperationException errorMsg)