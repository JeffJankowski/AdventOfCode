// Jeff Jankowski - 12/16/2015
// http://adventofcode.com/day/16

open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv = 
    let target = ["children",3; "cats",7; "samoyeds",2; "pomeranians",3; "akitas",0; "vizslas",0; 
                  "goldfish",5; "trees",3; "cars",2; "perfumes",1] |> Map.ofList

    let sues = 
        IO.File.ReadAllLines "..\..\input.txt"
        |> Array.map (fun s ->
           (Regex.Match(s, "^Sue (\d+)").Groups.[1].Value |> Int32.Parse,
            Regex.Match(s, "\d+: (.+), (.+), (.+)$").Groups
            |> Seq.cast<Capture>
            |> Seq.skip 1
            |> Seq.map (fun capt -> 
                let split = capt.Value.Split ' '
                (split.[0].Trim ':', Int32.Parse split.[1]))) )

    let f1 (t, n) = target.[t] = n
    let f2 (t, n) = 
        match t with
        | ("cats"|"trees") -> n > target.[t]
        | ("pomeranians"|"goldfish") -> n < target.[t]
        | _ -> n = target.[t]

    let find s f = 
        sues
        |> Array.filter (fun (_, traits) -> traits |> Seq.forall f)
        |> Array.get <| 0
        |> fst
        |> printfn "%s: %d" s

    find "Fake Sue" f1
    find "Real Sue" f2

    Console.Read ()
