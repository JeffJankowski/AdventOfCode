// Jeff Jankowski - 12/14/2015
// http://adventofcode.com/day/14

open System

let step (fly, rest, dist) ((name, spd, flyt, restt) : (string * int * int * int)) = 
    if fly < flyt then (fly+1, rest, dist + spd)
    elif rest < restt then (fly, rest+1, dist)
    else (1, 0, dist + spd)

let runDist deer = 
    [1..2503]
    |> List.fold (fun hist _ -> (step (hist |> List.head) deer :: hist) ) [(0,0,0)]
    |> List.map (fun (_,_,dist) -> dist)


let runPts (deers : (string * int * int * int)[]) = 
    let runs = deers |> Array.map (fun (name, spd, flyt, restt) -> 
        (name, runDist (name, spd, flyt, restt) |> List.rev |> List.toArray))

    [1..2503]
    |> List.fold (fun (scores : Map<string,int>) i ->
        let order = runs |> Array.map (fun (name, hist) -> (name, hist.[i])) |> Array.sortBy snd |> Array.rev
        let leaders = order |> Seq.takeWhile (fun (name, dist) -> dist = (snd order.[0]))
        leaders
        |> Seq.fold (fun sc (n,_) -> sc.Add (n, (sc.Item n)+1)) scores
        
        ) (deers |> Array.map (fun (n,_,_,_) -> (n, 0)) |> Map.ofArray)
            

[<EntryPoint>]
let main argv = 
    let map = 
       IO.File.ReadAllLines("..\..\input.txt")
        |> Array.map (fun s ->
            let split = s.Split(' ')
            let nums = split |> Array.filter (fun st -> st |> Seq.forall Char.IsDigit) |> Array.map Int32.Parse
            (split.[0], nums.[0], nums.[1], nums.[2]) )
            
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
