module FilterParser

open System
open FParsec

open Types

type Parser<'t> = Parser<'t, unit>

type FilterTag =
  | F of bool * DateTime               // from
  | T of bool * DateTime               // to
  | E of int * int option * int option // exact
  | A of string                        // account
  | V of string                        // virtual-account
  | P of string                        // payee
  | N of string                        // narrative
  | C of string                        // commodity
  | M of string                        // commodity measure
  | H of string                        // hastag

let glse xs pred = xs |> List.choose pred
                      |> List.tryLast

let nSpaces0 = skipMany (skipChar ' ')
let nSpaces1 = skipMany1 (skipChar ' ')

let cflag c = opt (pchar c) |>> Option.isSome

let pPartialDate0 =
  let sep = pchar '-'
  let pelt = opt (sep >>. pint32)
  tuple3 pint32 pelt pelt

let pPartialDate =
  let mk (y, mm, md) = DateTime (y, Option.defaultValue 1 mm, Option.defaultValue 1 md)
  pPartialDate0 |>> mk

let pFilterTags =
  let symbols = "><@=?%#"
  let stn = " \t\n"
  let nnlQ q = manySatisfy (function| x when x = q -> false
                                    |'\t'|'\n' -> false
                                    | _ -> true)
  let sQ = between (pchar ''') (pchar ''') (nnlQ ''')
  let dQ = between (pchar '"') (pchar '"') (nnlQ '"')
  let uQ = many1Chars2 (noneOf symbols) (noneOf stn)

  let text  = sQ <|> dQ <|> uQ

  let pFrom = pchar '>' >>. cflag '=' .>>. pPartialDate |>> F
  let pTo   = pchar '<' >>. cflag '=' .>>. pPartialDate |>> T
  let pExact = skipString "==" >>. pPartialDate0 |>> E
  let pPayee = pchar '@' >>. text |>> P
  let pNarr = pchar '?' >>. text |>> N
  let pCommod = pchar '%' >>. text |>> C
  let pMeasure = skipString "%%" >>. text |>> M
  let pTag = pchar '+' >>. text |>> H
  let pAcc = text |>> A

  let pelt = choice [pExact; pFrom; pTo; pPayee; pNarr; pMeasure; pCommod; pTag; pAcc]

  nSpaces0 >>. sepBy pelt nSpaces1 .>> eof

let pFilter : Parser<Filter> =
  let mk tags =
    let f = glse tags (function F (e, d)  -> Some (e,d)   | _ -> None)
    let t = glse tags (function T (e, d)  -> Some (e,d)   | _ -> None)
    let e = glse tags (function E (y,m,d) -> Some (y,m,d) | _ -> None)

    let a = tags |> List.choose (function A v -> Some v | _ -> None)
    let p = tags |> List.choose (function P v -> Some v | _ -> None)
    let n = glse tags (function N v -> Some v | _ -> None)
    let c = tags |> List.choose (function C v -> Some v | _ -> None)
    let m = tags |> List.choose (function M v -> Some v | _ -> None)
    let ts = tags |> List.choose (function H v -> Some v | _ -> None)

    {
      Timespan = match (f,t,e) with
                  | (None, None, None) -> None
                  | (_, _, Some e)     -> Some (Choice1Of2 e)
                  | _                  -> Some (Choice2Of2 (f,t))
      Accounts = a
      Payees = p
      Narrative = n
      Commodities = c
      Measures = m
      Tags = ts
    }

  pFilterTags |>> mk