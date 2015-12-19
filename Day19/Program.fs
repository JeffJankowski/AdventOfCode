// Jeff Jankowski - 12/19/2015
// http://adventofcode.com/day/19

open Microsoft.FSharp.Collections


type Step = {str: string; n: int;}


let convert (mol: string) (map: seq<string*string>) = 
    let spaces = 
        map
        |> PSeq.map fst
        |> PSeq.distinct
        |> PSeq.fold (fun (s:string) c -> s.Replace(c, sprintf " %s " c) ) mol
    spaces.Split (' ')
    |> Array.filter (fun s -> System.String.IsNullOrEmpty s |> not)


let replace map (mol:string) ((inp:string),(outp:string)) = 
    let map = (convert mol map)
    let idxs = 
        map
        |> PSeq.mapi (fun i e -> (i, e))
        |> PSeq.filter (fun (i,e) -> e = inp)
        |> PSeq.map fst
        |> PSeq.toArray
    seq {
        for i in idxs do
            let copy = Array.copy map
            copy.[i] <- outp
            yield System.String.Concat copy }

   



[<EntryPoint>]
let main argv = 
    //let mol = System.IO.File.ReadAllLines("..\..\molecule.txt").[0].Split(' ')
    let mol = System.IO.File.ReadAllLines("..\..\molecule.txt").[0]
    let input = System.IO.File.ReadAllLines "..\..\input.txt"

    let map = 
        input
        |> Seq.map (fun ln ->
            let split = ln.Split(' ')
            (split.[0], split.[2]) )

    let revmap = map |> Seq.map (fun (x,y) -> (y,x))
    
//    let rec step (n:int) start =
//        let d = 
//            map
//            |> Seq.map (fun (inp,outp) -> replace map start (inp,outp))
//            |> Seq.fold (fun s c -> s |> Seq.append c) Seq.empty
//            |> Seq.distinct
//        if d |> Seq.tryFind (fun s -> s = mol) |> Option.isSome then n
//        else
//            d
//            |> Seq.map (fun s -> step (n+1) s)
//            |> Seq.min

    let stack = new System.Collections.Generic.Stack<Step> ()
    stack.Push( {str=mol;n=0} )

    while stack.Count > 0 do
        let st = stack.Pop ()
//        printfn "%d:\t%s" st.n st.str
        //printfn "%d" st.str.Length
        if st.str = "e" then printfn "%d" st.n
        else 
            revmap
            |> PSeq.map (fun (inp,outp) -> replace revmap st.str (inp,outp))
            |> PSeq.fold (fun s c -> s |> Seq.append c) Seq.empty
            |> PSeq.distinct
            |> PSeq.iter (fun s -> stack.Push({str=s; n=st.n+1}))


//    dist
//    |> Seq.length
//    |> printfn "%A"

    System.Console.Read ()
