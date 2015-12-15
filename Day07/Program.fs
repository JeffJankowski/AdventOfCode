// Jeff Jankowski - 12/07/2015
// http://adventofcode.com/day/7

open System
open System.Collections.Generic

let rec emulate (dict : Dictionary<string, string[]>) (cache : Dictionary<string, uint16>) (target : string) = 
    if cache.ContainsKey (target) then cache.[target]
    else
        let recall = emulate dict cache 
        let res = 
            match (dict.[target]) with
            | [|a|] -> if Char.IsDigit a.[0] then UInt16.Parse a else recall a
            | [|_; a|] -> ~~~(recall a)
            | [|a; "AND"; b|] -> (if Char.IsDigit a.[0] then UInt16.Parse a else recall a) &&& (recall b)
            | [|a; "OR"; b|] -> (recall a) ||| (recall b)
            | [|a; "RSHIFT"; x|] -> (recall a) >>> Int32.Parse x
            | [|a; "LSHIFT"; x|] -> (recall a) <<< Int32.Parse x
            | _ -> 0us
        cache.[target] <- res
        res


[<EntryPoint>]
let main argv = 
    let input = IO.File.ReadLines ("..\..\input.txt")
    let dict = new Dictionary<string, string[]> ()
    let cache = new Dictionary<string, uint16> ()

    input
    |> Seq.iter (fun s -> 
        let spl = s.Split (' ') 
        dict.[spl.[spl.Length-1]] <- spl.[0..spl.Length-3] )

    emulate dict cache "a"
    |> printfn "%d"

    Console.Read ()
