// Jeff Jankowski - 12/12/2015
// http://adventofcode.com/day/12

open System
open FSharp.Data

let hasRed (record : (string * JsonValue)[]) =
    Array.exists (fun (k, v) -> 
        match v with
        | JsonValue.String s when s = "red" -> true
        | _ -> false ) record

let rec sum filt (jv : JsonValue) =
    match jv with
    | JsonValue.Number n -> int n
    | JsonValue.Record r -> 
        if (filt r) then 0
        else r |> Array.sumBy (fun (k, v) -> sum filt v)
    | JsonValue.Array a -> a |> Array.sumBy (fun e -> sum filt e)
    | _ -> 0

[<EntryPoint>]
let main argv = 
    let json = JsonValue.Parse (IO.File.ReadAllLines("..\..\input.txt").[0])
    printfn "All numbers:    %d" (sum (fun _ -> false) json)
    printfn "No red numbers: %d" (sum hasRed json)


    Console.Read ()
    
