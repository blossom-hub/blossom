module Shared

open System
open System.Text.RegularExpressions
open System.IO

open Types

let curry f a b = f (a,b)
let uncurry f (a, b) = f a b

let swap (a,b) = (b,a)

let first  f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let first3  f (a, b, c) = (f a, b, c)
let second3 f (a, b, c) = (a, f b, c)
let third3  f (a, b, c) = (a, b, f c)

let ( &&& ) f g a = (f a, g a)
let ( *** ) f g (a, b) = (f a, g b)

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let thd3 (_,_,c) = c

type idstring = string
let uid () : idstring =
  let gen() =
    let b = Path.GetRandomFileName()
    let dp = b.IndexOf('.')
    b.[..dp-1]
  gen()

let firstMatch pattern input =
  let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
  if (m.Success) then Some m.Groups.[1].Value else None

let (|FirstRegexGroup|_|) = firstMatch

let regexfilter p v =
  match v with
    | FirstRegexGroup p _ -> true
    | _ -> false

let iftrue b f a = if b then f a else a

module List =
  let groupByApply keyProjection valueProjection list =
    let grouped = list |> List.groupBy keyProjection
    let projected = grouped |> List.map (second valueProjection)
    projected

module Map =
  let merge (m1 : Map<'a, 'b>) (m2 : Map<'a, 'b>) = Map.fold (fun s k v -> Map.add k v s) m2 m1

  let mergeWith f (m1 : Map<'a, 'b>) (m2 : Map<'a, 'b>) =
    let left, jointL = Map.partition (fun k _ -> not(m2.ContainsKey(k))) m1
    let right = Map.filter (fun k _ -> not(m1.ContainsKey(k))) m2
    let resolved = jointL |> Map.map (fun k v -> f k v (m2.[k]))
    [left; right; resolved] |> List.collect Map.toList |> Map.ofList

module Set =
  let pop (v : 'a) (s : 'a Set) =
    if Set.contains v s
      then true, Set.remove v s
      else false, s


let makeSchedule tenor (left : DateTime) (right : DateTime) =
  let go g f x = (x, false) |> List.unfold (fun (d, flag) -> if flag then None else Some (f d, (g d, d >= right)))
                            |> Set.ofList
  let m1d (d : DateTime) = d.AddDays(-1.0)
  match tenor with
    | Y -> seq {left.Year + 1 .. 1 .. right.Year + 1 }
             |> Seq.map (fun y -> DateTime(y, 1, 1) |> m1d)
             |> Set.ofSeq
    | H -> if left.Month < 7 then DateTime(left.Year, 7, 1) else DateTime(left.Year+1, 1, 1)
             |> go (fun d -> d.AddMonths(6)) m1d
    | Q -> if left.Month < 3 then DateTime(left.Year, 4, 1)
             elif left.Month < 7 then DateTime(left.Year, 7, 1)
             elif left.Month < 10 then DateTime(left.Year, 10, 1)
             else DateTime(left.Year+1, 1, 1)
             |> go (fun d -> d.AddMonths(3)) m1d
    | M -> DateTime(left.Year, left.Month+1, 1)
             |> go (fun d -> d.AddMonths(1)) m1d
    | W d -> failwith "NYI"
    | D -> failwith "NYI"