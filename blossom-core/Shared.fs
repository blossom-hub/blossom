module Shared

open System
open System.IO

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


module List =
  let groupByApply keyProjection valueProjection list =
    let grouped = list |> List.groupBy keyProjection
    let projected = grouped |> List.map (second (List.map valueProjection))
    projected
