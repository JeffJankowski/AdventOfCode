// Jeff Jankowski - 12/17/2015
// http://adventofcode.com/day/17

open System


let rec choices = function
  | []      -> []
  | p::tail -> (p,tail) :: [ for (y,l) in choices tail -> (y,l) ];;

let rec combinations S k =
    [ if k=0 then yield [] else
            for (e,r) in choices S do
                for o in combinations r (k-1) do yield e::o  ];;

[<EntryPoint>]
let main argv = 
    let input = [33; 14; 18; 20; 45; 35; 16; 35; 1; 13; 18; 13; 50; 44; 48; 6; 24; 41; 30; 42]

    let combs = 
        [4..input.Length-1]
        |> List.map (fun n -> combinations input n)
        |> List.concat
        |> List.filter (fun ls -> ls |> List.sum = 150)
    
    let p2 = 
        combs
        |> Seq.groupBy (fun ls -> ls.Length)
        |> Seq.minBy fst

    printfn "%d" combs.Length
    printfn "%d" ((snd p2) |> Seq.length)

    Console.Read ()
