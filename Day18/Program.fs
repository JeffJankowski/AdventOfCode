// Jeff Jankowski - 12/18/2015
// http://adventofcode.com/day/18

open System


let neighbors (lights: bool[,]) (i, j) = 
    let mutable ct = 0
    ct <- ct + (try (if lights.[i-1,j-1] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i-1,j] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i-1,j+1] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i+1,j-1] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i+1,j] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i+1,j+1] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i,j-1] then 1 else 0) with | _ -> 0)
    ct <- ct + (try (if lights.[i,j+1] then 1 else 0) with | _ -> 0)
    ct

let step (lights: bool[,]) = 
    let (newl: bool[,]) = Array2D.zeroCreate 100 100

    for i = 0 to 99 do
        for j = 0 to 99 do
            let ct = neighbors lights (i, j)
            let curr = lights.[i,j]
            if not curr && ct = 3 then
                newl.[i,j] <- true
            elif curr && ct <> 2 && ct <> 3 then
                newl.[i,j] <- false
            else
                newl.[i,j] <- curr

    newl.[0,0] <- true
    newl.[0,99] <- true
    newl.[99,99] <- true
    newl.[99,0] <- true
    newl
            

[<EntryPoint>]
let main argv = 
    let input = IO.File.ReadAllLines "..\..\input.txt"

    let lights = 
        Array2D.init 100 100 (fun i j -> 
            let c = input.[i].Chars j
            match c with 
            | '.' -> false
            | _ -> true
        )

//    let lights = 
//        input 
//        |> Array.mapi (fun i s ->
//            s 
//            |> Seq.map (fun c -> 
//                match c with 
//                | '.' -> false
//                | _ -> true )
//            |> Seq.toArray )

    lights.[0,0] <- true
    lights.[0,99] <- true
    lights.[99,99] <- true
    lights.[99,0] <- true


    [0..99]
    |> Seq.fold (fun s _ -> step s) lights
    |> Seq.cast<bool>
    |> Seq.sumBy (fun b -> if b then 1 else 0)
    |> printfn "%d"

    Console.Read ()
