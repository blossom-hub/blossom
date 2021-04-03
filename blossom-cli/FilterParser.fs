module FilterParser

open System
open FParsec

open Types

type Parser<'t> = Parser<'t, unit>

type FilterTag =
  | F of bool * DateTime      // from
  | T of bool * DateTime      // to
  | A of string               // account
  | V of string               // virtual-account
  | P of string               // payee
  | N of string               // narrative
  | C of string               // commodity
  | D of string               // denominated commodity
  | H of string               // hastag

let glse xs pred = xs |> List.choose pred
                      |> List.tryLast

let nSpaces0 = skipMany (skipChar ' ')
let nSpaces1 = skipMany1 (skipChar ' ')

let cflag c = opt (pchar c) |>> Option.isSome

let pPartialDate =
  let sep = pchar '-'
  let pelt = opt (sep >>. pint32)
  let mk y mm md = DateTime (y, Option.defaultValue 1 mm, Option.defaultValue 1 md)
  pipe3 pint32 pelt pelt mk

let pFilterTags =
  let symbols = "><@=?%#/"
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
  let pPayee = pchar '@' >>. text |>> P
  let pNarr = pchar '?' >>. text |>> N
  let pCommod = pchar '%' >>. text |>> C
  let pDenom = skipString "%%" >>. text |>> D
  let pTag = pchar '+' >>. text |>> H
  let pVAcc = pchar '/' >>. text |>> V
  let pAcc = text |>> A

  let pelt = choice [pFrom; pTo; pPayee; pNarr; pDenom; pCommod; pTag; pVAcc; pAcc]

  nSpaces0 >>. sepBy pelt nSpaces1 .>> eof

let pFilter : Parser<Filter> =
  let mk tags =
    let f = glse tags (function F (e, d) -> Some (e,d) | _ -> None)
    let t = glse tags (function T (e, d) -> Some (e,d) | _ -> None)

    let a = tags |> List.choose (function A v -> Some v | _ -> None)
    let va = glse tags (function V v -> Some v | _ -> None)
    let p = tags |> List.choose (function P v -> Some v | _ -> None)
    let n = glse tags (function N v -> Some v | _ -> None)
    let c = tags |> List.choose (function C v -> Some v | _ -> None)
    let d = tags |> List.choose (function D v -> Some v | _ -> None)
    let ts = tags |> List.choose (function H v -> Some v | _ -> None)

    {
      Timespan = match (f,t) with (None, None) -> None | _ -> Some (f,t)
      Accounts = a
      VAccount = va
      Payees = p
      Narrative = n
      Commodities = c
      Denominations = d
      Tags = ts
    }

  pFilterTags |>> mk