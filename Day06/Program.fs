// Jeff Jankowski - 12/06/2015
// http://adventofcode.com/day/6

open System
type Op = Toggle = 2 | On = 1 | Off = -1
type Point = { x: int; y: int }
type Rect = { op: Op; p1: Point; p2: Point }

let convert (point : string) = 
    let both = point.Split(',')
    { x = Int32.Parse both.[0]; y = Int32.Parse both.[1] }

let parse (str : string) =
    let points = str.Split(' ') |> Array.filter (fun s -> Char.IsDigit s.[0])
    let opStr = String (str |> Seq.takeWhile (fun c -> not (Char.IsDigit c)) |> Seq.toArray)
    let op =
        match opStr with
        | s when s.Contains "on" -> Op.On
        | s when s.Contains "off" -> Op.Off
        | _ -> Op.Toggle
    { op = op; p1 = convert points.[0]; p2 = convert points.[1] }

let apply f (rect: Rect) = 
    let x = (Math.Min (rect.p1.x, rect.p2.x), Math.Max (rect.p1.x, rect.p2.x))
    let y = (Math.Min (rect.p1.y, rect.p2.y), Math.Max (rect.p1.y, rect.p2.y))
    Array2D.init ((snd x)-(fst x)+1) ((snd y)-(fst y)+1) (fun i j -> ((i+(fst x)),(j+(fst y))))
    |> Array2D.iter f

let applyO (lights : bool[,]) (rect: Rect) = 
    apply (fun cell -> 
        lights.[fst cell, snd cell] <- 
            match rect.op with 
            | Op.On -> true
            | Op.Off -> false
            | _ -> not lights.[fst cell, snd cell]) rect

let applyB (lights: int[,]) (rect: Rect) = 
    apply (fun cell -> 
        lights.[fst cell, snd cell] <- Math.Max (0, lights.[fst cell, snd cell] + (int rect.op))
        ) rect

let countO (lights : bool[,]) = 
    lights |> Seq.cast<bool> |> Seq.sumBy (fun on -> if on then 1 else 0)

let countB (lights : int[,]) = 
    lights |> Seq.cast<int> |> Seq.sum


[<EntryPoint>]
let main argv = 
    let lightsO = Array2D.create 1000 1000 false
    let lightsB = Array2D.create 1000 1000 0
    let ops = System.IO.File.ReadLines("..\..\input.txt") |> Seq.map parse |> Seq.cache

    ops |> Seq.iter (applyO lightsO)
    printfn "Lights On:  %d" (countO lightsO)

    ops |> Seq.iter (applyB lightsB)
    printfn "Brightness: %d" (countB lightsB)

    System.Console.Read()
