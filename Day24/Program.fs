// Jeff Jankowski - 12/24/2015
// http://adventofcode.com/day/24

open System
open Helpers

let minQE (nums: int list) grps =
    [2..(nums.Length/grps - 1)]
    |> List.map (fun n -> comb n nums)
    |> List.concat
    |> List.filter (fun cmb -> cmb |> List.sum = (nums |> List.sum) / grps)
    |> Seq.groupBy (fun cmb -> cmb.Length)
    |> Seq.minBy (fun (len,_) -> len) |> snd
    |> Seq.map (fun cmb -> cmb |> List.map int64 |> List.reduce (*))
    |> Seq.min

[<EntryPoint>]
let main argv = 
    let nums = IO.File.ReadAllLines "..\..\input.txt" |> Array.map Int32.Parse |> Array.toList
    minQE nums 3 |> printfn "3 Groups: %d"
    minQE nums 4 |> printfn "4 Groups: %d"

    Console.Read ()
