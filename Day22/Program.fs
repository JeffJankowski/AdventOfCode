// Jeff Jankowski - 12/22/2015
// http://adventofcode.com/day/22

open System

type Spell = | Missile | Drain | Shield | Poison | Recharge
type Player = { hit: int; mana: int; amr: int; effects: (Spell*int) list }
type Boss = { hit: int; dmg: int }

let eff (me: Player, boss: Boss) =
    let (nMe, nBoss) = 
        me.effects
        |> List.fold (fun (m, b) (spl,_) -> 
             match spl with
                | Shield -> ({m with amr = 7}, b)
                | Poison -> (m, {b with Boss.hit = b.hit - 3})
                | Recharge -> ({m with mana = m.mana + 101}, b)
            ) ({me with amr = 0}, boss)
    let decr = List.map (fun (spl,trns) -> (spl,trns-1)) >> List.filter (fun (_,trns) -> trns > 0)
    ({nMe with effects = decr nMe.effects}, nBoss)

let findMin hard (spells: (Spell*int)[]) (newMe:Player) (newBoss:Boss) =
    let min = ref Int32.MaxValue
    let rec sim (me:Player) (boss:Boss) (mana: int) = 
        if mana <= min.Value then
            let (me1, boss1) = eff ({me with hit = me.hit - (if hard then 1 else 0)}, boss)
            let choices = 
                spells |> Array.filter (fun (spl,cst) ->
                    cst <= me1.mana && (not (me1.effects |> List.exists (fun (s,_) -> s = spl))))
            if me1.hit > 0 then
                if boss1.hit <= 0 && mana < min.Value then min.Value <- mana
                elif choices.Length > 0 then
                    for (spell,cost) in choices do
                        let (me2, boss2) = 
                            match spell with
                            | Missile -> (me1, {boss1 with hit = boss1.hit - 4})
                            | Drain -> ({me1 with hit = me1.hit + 2}, {boss1 with hit = boss1.hit - 2})
                            | Shield | Poison -> ({me1 with effects = (spell, 6) :: me1.effects}, boss1)
                            | Recharge -> ({me1 with effects = (spell, 5) :: me1.effects}, boss1)     
                            |> (fun (m,b) -> ({m with mana = m.mana - cost}, b))
                            |> eff
                            |> (fun (m,b) -> ({m with hit = m.hit - Math.Max(b.dmg - m.amr, 1)}, b))
                        if boss2.hit <= 0 && mana+cost < min.Value then min.Value <- mana+cost
                        elif me2.hit > 0 then sim me2 boss2 (mana+cost)
    sim newMe newBoss 0
    min.Value
        

[<EntryPoint>]
let main argv = 
    
    let spells = 
        [|(Spell.Missile, 53); (Spell.Drain, 73); 
        (Spell.Shield, 113); (Spell.Poison, 173); (Spell.Recharge, 229);|]
    let me = {hit = 50; mana = 500; amr = 0; effects = List.empty<Spell*int>}
    let boss = {hit = 55; dmg = 8;} 
        
    printfn "Min Mana:  %d" (findMin false spells me boss)
    printfn "Hard Mode: %d" (findMin true spells me boss)

    Console.Read ()