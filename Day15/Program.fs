// Jeff Jankowski - 12/15/2015
// http://adventofcode.com/day/15

open System

type Type = Sugar | Sprinkles | Candy | Chocolate
type Ingredient =  { name: Type; capacity: int; durability: int; flavor: int; texture: int;
                     calories: int; }

let score (ingrs: (Ingredient*int) list) =
    let bottom n = Math.Max (n, 0)
    let cap = ingrs |> List.sumBy (fun (ingr, tsp) -> ingr.capacity * tsp) |> bottom
    let dur = ingrs |> List.sumBy (fun (ingr, tsp) -> ingr.durability * tsp) |> bottom
    let flv = ingrs |> List.sumBy (fun (ingr, tsp) -> ingr.flavor * tsp) |> bottom
    let tex = ingrs |> List.sumBy (fun (ingr, tsp) -> ingr.texture * tsp) |> bottom
    let cal = ingrs |> List.sumBy (fun (ingr, tsp) -> ingr.calories * tsp) //neg calories are a lie
    (cap * dur * flv * tex, cal)

[<EntryPoint>]
let main argv = 
    let ingreds = 
        [ {name = Sugar; capacity = 3; durability = 0; flavor = 0; texture = -3; calories = 2;};
          {name = Sprinkles; capacity = -3; durability = 3; flavor = 0; texture = 0; calories = 9;};
          {name = Candy; capacity = -1; durability = 0; flavor = 4; texture = 0; calories = 1;};
          {name = Chocolate; capacity = 0; durability = 0; flavor = -2; texture = 2; calories = 8;};
        ]

    let scores = 
        seq {
            for i = 1 to 100 do 
                for j = 1 to 100 - i do
                    for k = 1 to 100 - i - j do
                        let m = 100 - i - j - k
                        yield score (List.zip ingreds [i;j;k;m]) }
    
    scores
    |> Seq.map fst
    |> Seq.max
    |> printfn "Best Cookie:    %d"
                    
    scores
    |> Seq.filter (fun (_,cal) -> cal = 500)
    |> Seq.map (fun (s,_) -> s)
    |> Seq.max
    |> printfn "500-Cal Cookie: %d"


    Console.Read ()
