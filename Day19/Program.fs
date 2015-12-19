// Jeff Jankowski - 12/19/2015
// http://adventofcode.com/day/19

open System
open Microsoft.FSharp.Collections


type Step = {str: string; n: int;}

let replace map (mol:string) ((inp:string),(outp:string)) = 
    let chunked = 
        (map
        |> PSeq.map fst
        |> PSeq.distinct
        |> PSeq.fold (fun (s:string) c -> s.Replace(c, sprintf " %s " c) ) mol).Split ' '
        |> Array.filter (fun s -> not <| String.IsNullOrWhiteSpace s)
    let idxs = 
        chunked
        |> PSeq.mapi (fun i e -> (i, e))
        |> PSeq.filter (fun (i,e) -> e = inp)
        |> PSeq.map fst
        |> PSeq.toArray
    seq {
        for i in idxs do
            let copy = Array.copy chunked
            copy.[i] <- outp
            yield String.Concat copy }

let distinct map mol = 
    map
    |> PSeq.map (fun (i,o) -> replace map mol (i,o))
    |> PSeq.fold (fun s c -> s |> Seq.append c) Seq.empty
    |> PSeq.distinct


[<EntryPoint>]
let main argv = 
    let sw = Diagnostics.Stopwatch.StartNew ()

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

    sw.Stop ()
    printfn "\n%fms" sw.Elapsed.TotalMilliseconds

    System.Console.Read ()
