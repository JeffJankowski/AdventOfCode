// Jeff Jankowski - 12/09/2015
// http://adventofcode.com/day/9

open System
open System.Collections.Generic

// F# for Scientists (page 166-167)
//*********************************
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)
//*********************************


let calc (map : Map<string, seq<string*int>>) (path : string list) = 
    path
    |> List.toSeq
    |> Seq.pairwise
    |> Seq.sumBy (fun p -> 
        match map.[fst p] |> Seq.tryFind (fun (d, c) -> d = (snd p)) with
            | Some (d, c) -> c
            | None -> Int32.MinValue )


[<EntryPoint>]
let main argv =
    
    let map = 
        System.IO.File.ReadLines ("..\..\input.txt")
        |> Seq.map (fun (s : string) ->
            let split = s.Split (' ')
            [ (split.[0], (split.[2], System.Int32.Parse split.[4]));
              (split.[2], (split.[0], System.Int32.Parse split.[4])) ] )
        |> Seq.concat
        |> Seq.groupBy fst
        |> Seq.map (fun grp -> (fst grp, (snd grp) |> Seq.map snd))
        |> Map.ofSeq
    
    let perms = 
        map
        |> Map.toList
        |> List.map fst
        |> permute

    let costs = 
        perms
        |> List.map (fun l -> calc map l)
        |> List.filter (fun c -> c > 0)

    costs
    |> List.min
    |> printfn "Min: %d"

    costs
    |> List.max
    |> printfn "Max: %d"

    Console.Read ()