// Jeff Jankowski - 12/02/2015
// http://adventofcode.com/day/2


let dimensions (str : string) = 
    str.Split [|'x'|] 
        |> Seq.map System.Int32.Parse 
        |> Seq.toArray

let smallest dims n =
    dims 
        |> Seq.sort 
        |> Seq.take n 
        |> Seq.toArray


let paper (str : string) = 
    let dims = dimensions str
    let small = smallest dims 2 
    2*dims.[0]*dims.[1] + 2*dims.[1]*dims.[2] + 2*dims.[2]*dims.[0] + small.[0]*small.[1]


let ribbon (str : string) = 
    let dims = dimensions str
    let small = smallest dims 2 
    2*small.[0] + 2*small.[1] + dims.[0]*dims.[1]*dims.[2]


[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadLines("..\..\input.txt")
   
    input
    |> Seq.map paper
    |> Seq.sum
    |> printfn "Total Paper: %d"

    input
    |> Seq.map ribbon
    |> Seq.sum
    |> printfn "Total Ribbon: %d"


    System.Console.Read()