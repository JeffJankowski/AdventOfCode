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
    ({nMe with effects = nMe.effects |> List.map (fun (spl,trns) -> (spl,trns-1)) |> List.filter (fun (_,trns) -> trns > 0)}, nBoss)


let rec sim (spells: (Spell*int)[]) (me:Player) (boss:Boss) (mana: int) = 
    seq {
        //apply pending spells for my turn
        let (me1, boss1) = eff (me, boss)

        let choices = 
            spells
            |> Array.filter (fun (spl,cst) ->
                cst <= me1.mana && (not (me1.effects |> List.exists (fun (s,_) -> s = spl))))

        if boss1.hit <= 0 then yield mana
        elif choices.Length = 0 then yield Int32.MaxValue
        else
            //choose a spell
            for choice in choices do
                let spell = fst choice
                let cost = snd choice
            
                let (me2, boss2) = 
                    match spell with                                                                                    //apply immediate
                    | Missile -> (me1, {boss1 with hit = boss1.hit - 4})
                    | Drain -> ({me1 with hit = me1.hit + 2}, {boss1 with hit = boss1.hit - 2})
                    | Shield | Poison -> ({me1 with effects = me1.effects |> List.append [(spell, 6)]}, boss1)          //add effects
                    | Recharge -> ({me1 with effects = me1.effects |> List.append [(spell, 5)]}, boss1)     
                    |> (fun (m,b) -> ({m with mana = m.mana - cost}, b))                                                //reduce mana cost
                    |> eff                                                                                              //apply effects
                    |> (fun (m,b) -> ({m with hit = m.hit - Math.Max(b.dmg - m.amr, 1)}, b))                            //boss attack

                if boss2.hit <= 0 then yield mana+cost
                elif me2.hit <= 0 then 
                    yield Int32.MaxValue
                else yield! sim spells me2 boss2 (mana+cost)
    }

        

[<EntryPoint>]
let main argv = 
    
    let spells = 
        [|(Spell.Missile, 53); (Spell.Drain, 73); 
        (Spell.Shield, 113); (Spell.Poison, 173); (Spell.Recharge, 229);|]
    let me = {hit = 50; mana = 500; amr = 0; effects = List.empty<Spell*int>}
    let boss = {hit = 55; dmg = 8;} 
        
    sim spells me boss 0
    |> Seq.min
    |> printfn "%d"
    

    Console.Read ()