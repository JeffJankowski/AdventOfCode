// Jeff Jankowski - 12/14/2015
// http://adventofcode.com/day/14

open System

type Deer = { name: string; spd: int; flyT: int; restT: int }

let step (fly, rest, dist) (deer: Deer) = 
    if fly < deer.flyT then (fly+1, rest, dist + deer.spd)
    elif rest < deer.restT then (fly, rest+1, dist)
    else (1, 0, dist + deer.spd)

let runDist deer = 
    [1..2503]
    |> List.fold (fun hist _ -> (step (hist |> List.head) deer :: hist) ) [(0,0,0)]
    |> List.map (fun (_,_,dist) -> dist)

let runPts (deers : Deer[]) = 
    let runs = deers |> Array.map (fun d -> (d.name, runDist d |> List.rev |> List.toArray))
    [1..2503]
    |> List.fold (fun (scores : Map<string,int>) i ->
        let order = runs |> Array.map (fun (n, h) -> (n, h.[i])) |> Array.sortBy snd |> Array.rev
        order 
        |> Seq.takeWhile (fun (_,dist) -> dist = (snd order.[0]))
        |> Seq.fold (fun sc (n,_) -> sc.Add (n, (sc.Item n)+1)) scores
        ) (deers |> Array.map (fun d -> (d.name, 0)) |> Map.ofArray)
            

[<EntryPoint>]
let main argv = 
    let map = 
        IO.File.ReadAllLines("..\..\input.txt")
        |> Array.map (fun s ->
            let split = s.Split(' ')
            let nums = 
                split 
                |> Array.filter (fun st -> st |> Seq.forall Char.IsDigit) 
                |> Array.map Int32.Parse
            { name = split.[0]; spd = nums.[0]; flyT = nums.[1]; restT = nums.[2] } )
            
    map
    |> Array.map (fun d -> runDist d |> List.head)
    |> Array.max
    |> printfn "%d"

    runPts map
    |> Map.toList
    |> List.map snd
    |> List.max
    |> printfn "%d"

    Console.Read ()
