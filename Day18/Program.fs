// Jeff Jankowski - 12/18/2015
// http://adventofcode.com/day/18


let corners (lights: bool[,]) = 
    for x in [0;99] do
        for y in [0;99] do
            lights.[x,y] <- true
    lights

let neighbors (lights: bool[,]) (i, j) = 
    let contains n = n >= 0 && n < 100
    let mutable ct = 0
    for x in [i-1..i+1] do
        for y in [j-1..j+1] do
            if contains x && contains y && lights.[x,y] then ct <- ct + 1
    if lights.[i,j] then ct-1 else ct

let step (lights: bool[,]) f = 
    let (newl: bool[,]) = Array2D.zeroCreate 100 100
    [0..newl.Length-1]
    |> Microsoft.FSharp.Collections.PSeq.iter (fun n ->
        let (i, j) = (n % 100, n / 100)
        newl.[i,j] <-
            match (lights.[i,j], neighbors lights (i, j)) with
            | (true, ct) when ct <> 2 && ct <> 3 -> false
            | (false, 3) -> true
            | (on, _) -> on )
    f newl
            
[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines "..\..\input.txt"
    let lights = 
        Array2D.init 100 100 (fun i j -> 
            match input.[i].Chars j with 
            | '.' -> false
            | _ -> true )

    let run f = 
        [0..99] 
        |> Seq.fold (fun s _ -> step s f)(f lights) 
        |> Seq.cast<bool> 
        |> Seq.sumBy (fun b -> if b then 1 else 0)
    
    run (fun a -> a) |> printfn "100 Steps: %d"
    run corners      |> printfn "4 Corners: %d"
    

    System.Console.Read ()
