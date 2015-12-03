// Jeff Jankowski - 12/03/2015
// http://adventofcode.com/day/3


let move (curr, l) dir =
    let next = 
        match dir with 
        | '^' -> (fst curr, snd curr + 1)
        | '<' -> (fst curr - 1, snd curr) 
        | 'v' -> (fst curr, snd curr - 1) 
        | '>' -> (fst curr + 1, snd curr) 
        | _ -> (fst curr, snd curr) 
    (next, next :: l)


[<EntryPoint>]
let main argv = 
    let dirs = System.IO.File.ReadLines("..\..\input.txt") |> Seq.head
    
    let (_, houses) = dirs |> Seq.fold move ((0,0), [(0,0)])
    houses
    |> Seq.distinct
    |> Seq.length
    |> printfn "Distinct Houses: %d"

    let dirsI = dirs |> Seq.mapi (fun i e -> (i, e))
    let (_, human) = 
        dirsI
        |> Seq.filter (fun (i, e) -> i % 2 = 0)
        |> Seq.map snd
        |> Seq.fold move ((0,0), [(0,0)])
    let (_, robot) = 
        dirsI
        |> Seq.filter (fun (i, e) -> i % 2 = 1)
        |> Seq.map snd
        |> Seq.fold move ((0,0), [(0,0)])
    human
    |> Seq.append robot
    |> Seq.distinct
    |> Seq.length
    |> printfn "Distinct Houses (with robot): %d"

    System.Console.Read()
