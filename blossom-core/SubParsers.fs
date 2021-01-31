module SubParsers

open System
open FParsec

open Types
open JournalParser

type FilterTag =
  | F of bool * DateTime
  | T of bool * DateTime
  | A of string
  | SA of string
  | P of string
  | N of string
  | C of string
  | Ht of string

let cflag c = opt (pchar c) |>> Option.isSome

let pPartialDate =
  let sep = pchar '-'
  let pelt = opt (sep >>. pint32)
  let mk y mm md = DateTime (y, Option.defaultValue 1 mm, Option.defaultValue 1 md)
  pipe3 pint32 pelt pelt mk

let pFilterTags =
  let symbols = "><@?=*"
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
  let pHashTag = pchar '#' >>. text |>> Ht
  let pSubAcc = pchar '/' >>. text |>> SA
  let pAcc = text |>> A

  let pelt = choice [pFrom; pTo; pPayee; pNarr; pCommod; pHashTag; pSubAcc; pAcc]

  nSpaces0 >>. sepBy pelt nSpaces1 .>> eof

let pFilter =
  let mk tags =
    let f = glse tags (function F (e, d) -> Some (e,d) | _ -> None)
    let t = glse tags (function T (e, d) -> Some (e,d) | _ -> None)

    let a = glse tags (function A v -> Some v | _ -> None)
    let sa = glse tags (function SA v -> Some v | _ -> None)
    let p = glse tags (function P v -> Some v | _ -> None)
    let n = glse tags (function N v -> Some v | _ -> None)
    let c = glse tags (function C v -> Some v | _ -> None)
    let hs = tags |> List.choose (function Ht v -> Some v | _ -> None) |> set

    {
      between = match (f,t) with (None, None) -> None | _ -> Some (f,t)
      account = a
      subaccount = sa
      payee = p
      narrative = n
      commodity = c
      hashtags = hs
    }

  pFilterTags |>> mk

let pTenor : Parser<Tenor, unit>  =
  let z = anyOf "YMHQ"
           |>> function | 'Y' -> Y
                        | 'M' -> M
                        | 'H' -> H
                        | 'Q' -> Q
                        | _   -> Y  // which shouldn't happen by design
  let w = pchar 'W' >>. opt pint32 |>> function Some i -> W (enum i) | _ -> W DayOfWeek.Friday
  (w <|> z)