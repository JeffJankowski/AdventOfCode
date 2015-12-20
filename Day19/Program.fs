// Jeff Jankowski - 12/19/2015
// http://adventofcode.com/day/19

open System
open System.Text.RegularExpressions

let replace (mol:string) ((inp:string),(outp:string)) = 
    Regex.Matches(mol, inp)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> 
        sprintf "%s%s%s" (mol.Substring(0, m.Index)) outp (mol.Substring(m.Index+m.Length)) )


[<EntryPoint>]
let main argv = 
    let mol = System.IO.File.ReadAllLines("..\..\molecule.txt").[0]
    let map = 
        IO.File.ReadAllLines "..\..\input.txt"
        |> Seq.map (fun ln ->
            let split = ln.Split(' ')
            (split.[0], split.[2]) )

    let distinct mol = 
        map
        |> Seq.map (fun (i,o) -> replace mol (i,o))
        |> Seq.fold (fun s c -> s |> Seq.append c) Seq.empty
        |> Seq.distinct
    printfn "Distinct:   %d" (distinct mol |> Seq.length)

    let revmap = map |> Seq.map (fun (x,y) -> (y,x)) |> Seq.sortBy (fun (i,_) -> -i.Length) 
    let rec step curr cnt = //greedy
        if curr = "e" then cnt
        else
            let (ncurr,nn) = revmap |> Seq.fold (fun (s:string, n:int) (i,o) -> 
                let cmb = replace s (i,o) 
                if cmb |> Seq.isEmpty then (s, n)
                else (Seq.head cmb, n+1)) (curr,cnt)
            step ncurr nn
    printfn "Steps to e: %d" (step mol 0)

    System.Console.Read ()
