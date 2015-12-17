// Jeff Jankowski - 12/17/2015
// http://adventofcode.com/day/17

open Helpers

[<EntryPoint>]
let main argv = 
    let combs = 
        [33; 14; 18; 20; 45; 35; 16; 35; 1; 13; 18; 13; 50; 44; 48; 6; 24; 41; 30; 42]
        |> powerset
        |> Seq.filter (fun lst -> lst |> List.sum = 150)

    let min = 
        combs
        |> Seq.groupBy (fun lst -> lst.Length)
        |> Seq.minBy fst

    printfn "150L Combos: %d" (combs |> Seq.length)
    printfn "Min Combos:  %d" ((snd min) |> Seq.length)

    System.Console.Read ()
