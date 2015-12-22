// Jeff Jankowski - 12/21/2015
// http://adventofcode.com/day/21

open System
open Helpers

type Player = {hit: int; dmg: int; amr: int;}

let rec sim (me:Player) (boss:Player) = 
    let nBoss = {boss with hit = boss.hit - Math.Max(me.dmg - boss.amr, 1)}
    let nMe = {me with hit = me.hit - Math.Max(boss.dmg - me.amr, 1)}
    if nBoss.hit <= 0 then true
    elif nMe.hit <= 0 then false
    else sim nMe nBoss

[<EntryPoint>]
let main argv = 
    let buy = 
        IO.File.ReadAllLines "..\..\input.txt"
        |> Array.map (fun s ->
            let split = s.Split '\t'
            (split.[0], Int32.Parse split.[1], Int32.Parse split.[2], Int32.Parse split.[3]) )
        |> Array.toList

    let boss = {hit = 103; dmg = 9; amr = 2;}
    let perms = 
        powerset buy |> Seq.toList 
        |> List.filter (fun items -> 
            let (w,a,r) = items |> List.fold (fun (w,a,r) (n,_,_,_) -> 
                match n with 
                | "W" -> (w+1,a,r)
                | "A" -> (w,a+1,r)
                | "R" -> (w,a,r+1)) (0,0,0)
            w = 1 && a <= 1 && r <= 2 )

    let run sf wf = 
        perms
        |> List.map (fun items -> 
            let (c,d,a) = items |> List.fold (fun (tc,td,ta) (_,c,d,a) -> (tc+c,td+d,ta+a)) (0,0,0)
            (c, {hit= 100; dmg= d; amr= a;}) )
        |> List.sortBy sf
        |> List.find (fun (_, me) -> wf <| sim me boss )
        |> fst
    
    printfn "Least Gold: %d" (run fst id)
    printfn "Most Gold:  %d" (run (fun (c,_) -> -c) not)

    Console.Read ()