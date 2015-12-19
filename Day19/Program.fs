// Jeff Jankowski - 12/19/2015
// http://adventofcode.com/day/19

open System
open Microsoft.FSharp.Collections
open System.Text.RegularExpressions

type Step = {str: string; n: int;}

let replace (mol:string) ((inp:string),(outp:string)) = 
    Regex.Matches(mol, inp)
    |> PSeq.cast<Match>
    |> PSeq.map (fun m -> 
        sprintf "%s%s%s" (mol.Substring(0, m.Index)) outp (mol.Substring(m.Index+m.Length)) )

let distinct map mol = 
    map
    |> PSeq.map (fun (i,o) -> replace mol (i,o))
    |> PSeq.fold (fun s c -> s |> Seq.append c) Seq.empty
    |> PSeq.distinct

[<EntryPoint>]
let main argv = 
    let mol = System.IO.File.ReadAllLines("..\..\molecule.txt").[0]
    let map = 
        IO.File.ReadAllLines "..\..\input.txt"
        |> Seq.map (fun ln ->
            let split = ln.Split(' ')
            (split.[0], split.[2]) )
    
    printfn "Distinct:     %d" (distinct map mol |> PSeq.length)

    let revmap = map |> Seq.map (fun (x,y) -> (y,x))
    let stack = new Collections.Generic.Stack<Step> ()
    stack.Push( {str=mol;n=0} )

    let mutable n = Option.None
    while stack.Count > 0 && n.IsNone do
        let st = stack.Pop ()
        if st.str = "e" then n <- Some(st.n)
        else 
            distinct revmap st.str
            |> PSeq.iter (fun s -> stack.Push({str=s; n=st.n+1}))
    printfn "Steps to 'e': %d" n.Value


    System.Console.Read ()
