// Jeff Jankowski - 12/11/2015
// http://adventofcode.com/day/11

let groupEqual xs = 
    List.foldBack (fun x acc -> 
            match acc, x with
            | [], _ -> [[x]]
            | (h :: t) :: rest, x when h = x -> (x :: h :: t) :: rest
            | acc, x -> [x] :: acc) xs []

let isgood (str : string) = 
    str |> Seq.forall (fun c -> c <> 'i' && c <> 'o' && c <> 'l') &&
    (str |> Seq.toList |> groupEqual |> List.filter (fun l -> l.Length >= 2) |> List.length) >= 2 &&
    str |> Seq.windowed 3 |> Seq.exists (fun a -> char (int a.[0] + 1) = a.[1] && char (int a.[1] + 1) = a.[2])

let rec incr (str : string) = 
    let sub = str.[0..str.Length-2]
    match str.[str.Length-1] with
    | 'z' -> sprintf "%s%c" (incr sub) 'a'
    | c -> sprintf "%s%c" sub (char (int c + 1))


[<EntryPoint>]
let main argv = 
    
    // Mutable state is much, much, much faster than the infinite sequence..
    let mutable input = "cqjxjnds"
    while not (isgood input) do
        input <- incr input
    printfn "%s" input

//    Seq.initInfinite (fun i -> i)
//    |> Seq.scan (fun last _ -> incr last) "cqjxjnds"
//    |> Seq.find (fun pass -> isgood pass)
//    |> printfn "%s"

    System.Console.Read ()
