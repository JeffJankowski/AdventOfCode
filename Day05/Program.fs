// Jeff Jankowski - 12/05/2015
// http://adventofcode.com/day/5


let filter1 (str : string) =  
    (str 
    |> Seq.filter (fun c -> c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
    |> Seq.length >= 3)
    && (str 
    |> Seq.pairwise 
    |> Seq.exists (fun p -> (fst p) = (snd p)))
    && (not (str.Contains("ab")) && not (str.Contains("cd")) && not (str.Contains("pq")) && not (str.Contains("xy")))

let filter2 (str : string) =  
    (str
    |> Seq.pairwise
    |> Seq.mapi (fun i e -> (i, e))
    |> Seq.groupBy (fun pairi -> snd pairi)
    |> Seq.exists (fun (key, grp) -> 
        let same = grp |> Seq.map fst |> Seq.sortBy (fun i -> i) |> Seq.toArray
        not (same.[same.Length-1] - same.[0] = (same.Length-1))))
    && (str
    |> Seq.windowed 3
    |> Seq.exists (fun arr -> arr.[0] = arr.[2]))
    

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadLines("..\..\input.txt")

    input 
    |> Seq.filter filter1
    |> Seq.length
    |> printfn "First Rules:  %d"

    input 
    |> Seq.filter filter2
    |> Seq.length
    |> printfn "Second Rules: %d"

    System.Console.Read()
