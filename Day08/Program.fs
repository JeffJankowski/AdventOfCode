// Jeff Jankowski - 12/08/2015
// http://adventofcode.com/day/8

open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv = 
    let input = IO.File.ReadLines ("..\..\input.txt")
    let literals = input |> Seq.sumBy (fun s -> s.Length)

    literals - (input |> Seq.sumBy (fun s -> (Regex.Unescape (s.Substring (1, s.Length - 2))).Length))
    |> printfn "Unesecaped difference: %d"
    
    (input |> Seq.sumBy (fun s -> 
        (Regex.Escape s).Length + (s |> Seq.filter (fun c -> c = '"') |> Seq.length) + 2)) - literals
    |> printfn "Escaped difference:    %d"

    Console.Read ()
