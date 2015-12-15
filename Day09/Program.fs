// Jeff Jankowski - 12/09/2015
// http://adventofcode.com/day/9

open System
open System.Collections.Generic
open Helpers

let calc (map : Map<string, seq<string*int>>) (path : string list) = 
    path
    |> List.toSeq
    |> Seq.pairwise
    |> Seq.sumBy (fun p -> 
        match map.[fst p] |> Seq.tryFind (fun (d, _) -> d = (snd p)) with
            | Some (_, c) -> c
            | None -> Int32.MinValue )


[<EntryPoint>]
let main argv =
    
    let map = 
        System.IO.File.ReadLines ("..\..\input.txt")
        |> Seq.map (fun (s : string) ->
            let split = s.Split (' ')
            [ (split.[0], (split.[2], Int32.Parse split.[4]));
              (split.[2], (split.[0], Int32.Parse split.[4])) ] )
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