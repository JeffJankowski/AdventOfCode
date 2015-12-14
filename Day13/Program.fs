// Jeff Jankowski - 12/13/2015
// http://adventofcode.com/day/13

open System
open Helpers

let calc (map : (Map<(string*string),int>)) (order : string list) = 
    order 
    |> List.mapi (fun i e -> (i, e))
    |> List.sumBy (fun (i,e) ->
        let left = if i = 0 then order.[order.Length-1] else order.[i-1]
        let right = order.[(i+1) % order.Length]
        (if (map.ContainsKey (e,left)) then map.Item (e,left) else 0) + 
        (if (map.ContainsKey (e,right)) then map.Item (e,right) else 0) )

let getmax map people = 
        people
        |> permute
        |> List.map (fun perm -> calc map perm)
        |> List.max
    
[<EntryPoint>]
let main argv = 
    let map = 
        IO.File.ReadAllLines("..\..\input.txt")
        |> Array.map (fun s -> 
            let split = s.Split (' ')
            let idx = split |> Array.findIndex (fun st -> st |> Seq.forall Char.IsDigit)
            let value = 
                match split.[idx-1] with
                | "lose" -> Int32.Parse(split.[idx]) * -1
                | _ -> Int32.Parse(split.[idx])
            ((split.[0], split.[split.Length - 1].TrimEnd ('.')), value) )
        |> Map.ofArray

    let people = map |> Map.toSeq |> Seq.map (fun (k, v) -> fst k) |> Seq.distinct |> Seq.toList
    people |> getmax map |> printfn "Without Me: %d"
    ("Jeff" :: people) |> getmax map |> printfn "With Me:    %d"


    Console.Read ()