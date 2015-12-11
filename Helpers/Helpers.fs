module Helpers

// F# for Scientists (page 166-167)
//*********************************
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)
//*********************************

let groupEqual xs = 
    List.foldBack (fun x acc -> 
            match acc, x with
            | [], _ -> [[x]]
            | (h :: t) :: rest, x when h = x -> (x :: h :: t) :: rest
            | acc, x -> [x] :: acc) xs []


let readInput = System.IO.File.ReadLines ("..\..\input.txt")
