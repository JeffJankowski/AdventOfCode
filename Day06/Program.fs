// Jeff Jankowski - 12/06/2015
// http://adventofcode.com/day/6

open System
type Op = Toggle = 2 | On = 1 | Off = -1

let convert (point : string) = 
    let both = point.Split(',')
    (Int32.Parse both.[0], Int32.Parse both.[1])

let parse (str : string) =
    let points = str.Split(' ') |> Array.filter (fun s -> Char.IsDigit s.[0])
    let op = str |> Seq.takeWhile (fun c -> not (Char.IsDigit c)) |> Seq.toArray 
    match String op with
    | s when s.Contains "toggle" -> (Op.Toggle, convert points.[0], convert points.[1])
    | s when s.Contains "on" -> (Op.On, convert points.[0], convert points.[1])
    | s when s.Contains "off" -> (Op.Off, convert points.[0], convert points.[1])
    | _ -> (Op.Toggle, (0,0), (0,0) )

let apply f lights ((op : Op), (p1 : (int * int)), (p2 : (int * int))) = 
    let x = (Math.Min (fst p1, fst p2), Math.Max (fst p1, fst p2))
    let y = (Math.Min (snd p1, snd p2), Math.Max (snd p1, snd p2))
    Array2D.init ((snd x)-(fst x)+1) ((snd y)-(fst y)+1) (fun i j -> ((i+(fst x)),(j+(fst y))))
    |> Array2D.iter f

let applyO (lights : bool[,]) ((op : Op), (p1 : (int * int)), (p2 : (int * int))) = 
    apply (fun cell -> lights.[fst cell, snd cell] <- match op with 
        | Op.Toggle -> not lights.[fst cell, snd cell]
        | Op.On -> true
        | Op.Off -> false
        | _ -> lights.[fst cell, snd cell]) lights (op, p1, p2)

let applyB (lights : int[,]) ((op : Op), (p1 : (int * int)), (p2 : (int * int))) = 
    apply  (fun cell -> lights.[fst cell, snd cell] <- Math.Max (0, lights.[fst cell, snd cell] + (int op))) lights (op, p1, p2)

let countO (lights : bool[,]) = 
    let c = ref 0
    lights |> Array2D.iter (fun b -> c := !c + (if b then 1 else 0))
    !c

let countB (lights : int[,]) = 
    let c = ref 0
    lights |> Array2D.iter (fun b -> c := !c + b)
    !c


[<EntryPoint>]
let main argv = 
    let lightsO = Array2D.init 1000 1000 (fun x y -> false)
    let lightsB = Array2D.init 1000 1000 (fun x y -> 0)
    let ops = System.IO.File.ReadLines("..\..\input.txt") |> Seq.map parse |> Seq.cache

    ops |> Seq.iter (applyO lightsO)
    printfn "Lights On:  %d" (countO lightsO)

    ops |> Seq.iter (applyB lightsB)
    printfn "Brightness: %d" (countB lightsB)

    System.Console.Read()
