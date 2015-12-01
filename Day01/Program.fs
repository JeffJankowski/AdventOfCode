// Jeff Jankowski - 12/01/2015
// http://adventofcode.com/day/1

let mapToInt = 
    function 
    | '(' -> 1
    | ')' -> -1
    | _ -> 0


[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadLines("..\..\input.txt")

    input 
    |> Seq.head 
    |> Seq.map mapToInt 
    |> Seq.sum
    |> printfn "Ending Floor: %d" 

    input 
    |> Seq.head 
    |> Seq.map mapToInt 
    |> Seq.scan (fun floor curr -> floor + curr) 0
    |> Seq.findIndex (fun floor -> floor < 0)
    |> printfn "First Basement Position: %d" 

    System.Console.Read()
