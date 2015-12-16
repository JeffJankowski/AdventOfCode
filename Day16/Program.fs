// Jeff Jankowski - 12/16/2015
// http://adventofcode.com/day/16

open System

type Information = 
    { children: int option;
      cats: int option;
      samoyeds: int option;
      pomeranians: int option;
      akitas: int option;
      vizslas: int option;
      goldfish: int option;
      trees: int option;
      cars: int option;
      perfumes: int option; }

let toNum (s: string) = 
    s.Trim ',' |> Int32.Parse

let mapInfo (s: string) = 
    let split = s.Split ' '
    let id = split.[1].Trim(':') |> Int32.Parse
    let num i = Some(split.[i] |> toNum)
    (id, [2;4;6]
    |> List.fold (fun s c -> 
        match split.[c].Trim(':') with
        | "children" -> {s with children = num (c+1)}
        | "cats" -> {s with cats = num (c+1)}
        | "samoyeds" -> {s with samoyeds = num (c+1)}
        | "pomeranians" -> {s with pomeranians = num (c+1)}
        | "akitas" -> {s with akitas = num (c+1)}
        | "vizslas" -> {s with vizslas = num (c+1)}
        | "goldfish" -> {s with goldfish = num (c+1)}
        | "trees" -> {s with trees = num (c+1)}
        | "cars" -> {s with cars = num (c+1)}
        | "perfumes" -> {s with perfumes = num (c+1)}
        ) {children = None; cats = None; samoyeds = None; pomeranians = None; akitas = None; vizslas = None; goldfish = None; trees = None; cars = None; perfumes = None;} )
    
let compare (target: Information) (search: Information) = 
    let ak = search.akitas.IsNone || search.akitas.Value = target.akitas.Value
    let car = search.cars.IsNone || search.cars.Value = target.cars.Value
    let cat = search.cats.IsNone || search.cats.Value > target.cats.Value
    let ch = search.children.IsNone || search.children.Value = target.children.Value
    let go = search.goldfish.IsNone || search.goldfish.Value < target.goldfish.Value
    let pe = search.perfumes.IsNone || search.perfumes.Value = target.perfumes.Value
    let po = search.pomeranians.IsNone || search.pomeranians.Value < target.pomeranians.Value
    let sa = search.samoyeds.IsNone || search.samoyeds.Value = target.samoyeds.Value
    let tr = search.trees.IsNone || search.trees.Value > target.trees.Value
    let vi = search.vizslas.IsNone || search.vizslas.Value = target.vizslas.Value
    ak && car && cat && ch && go && pe && po && sa && tr && vi

[<EntryPoint>]
let main argv = 
    let input = IO.File.ReadAllLines "..\..\input.txt"
    let map =
        input
        |> Array.map mapInfo
        
    let target = {children = Some(3); cats = Some(7); samoyeds = Some(2); pomeranians = Some(3); akitas = Some(0); vizslas = Some(0); goldfish = Some(5); trees = Some(3); cars = Some(2); perfumes = Some(1);}
    let cmp = compare target

    let filt = map |> Array.filter (fun (id, info) -> cmp info)
    printfn "Real Aunt: %d" (fst filt.[0])


    Console.Read ()
