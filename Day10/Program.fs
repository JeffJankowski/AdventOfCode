// Jeff Jankowski - 12/10/2015
// http://adventofcode.com/day/10

let calculate input =
    let (_, sb) =
        "0"
        |> Seq.append input
        |> Seq.pairwise
        |> Seq.fold (fun (cnt, (sb : System.Text.StringBuilder)) (x, y) -> 
            if (x = y) then (cnt+1, sb)
            else (1, sb.AppendFormat ("{0}{1}", cnt, x))
            ) (1, new System.Text.StringBuilder ())
    sb.ToString ()


[<EntryPoint>]
let main argv = 
    let forty = {1..50} |> Seq.fold (fun last _ -> calculate last) "3113322113"
    printfn "%d" forty.Length

    System.Console.Read ()
