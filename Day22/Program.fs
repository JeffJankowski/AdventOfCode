// Jeff Jankowski - 12/22/2015
// http://adventofcode.com/day/22

open System

type Spell = 
    | Missile 
    | Drain 
    | Shield
    | Poison
    | Recharge

type Player = { 
    mutable hit: int;
    mutable mana: int;
    mutable amr: int;
    mutable effects: (Spell*int) list }

type Boss = {
    mutable hit: int;
    dmg: int }

let apply (me: Player) (boss: Boss) =
    me.effects <-
        me.effects
        |> List.map (fun (spl,trns) ->
            match spl with
            | Shield -> me.amr <- if trns > 0 then 7 else 0
            | Poison -> boss.hit <- boss.hit - 3
            | Recharge -> me.mana <- me.mana + 101
            (spl, trns-1) )
        |> List.filter (fun (spl,trns) ->
            match spl with
            | Shield -> trns >= 0
            | Poison | Recharge -> trns > 0 )


let _rand = Random ()
let rec sim (spells: (Spell*int)[]) (me:Player) (boss:Boss) (mana: int) = 
    //apply pending spells for my turn
    apply me boss

    let choices = 
        spells
        |> Array.filter (fun (spl,cst) ->
            cst <= me.mana && (not (me.effects |> List.exists (fun (s,_) -> s = spl))))

    if boss.hit <= 0 then mana
    elif choices.Length = 0 then 
        Int32.MaxValue
    else
        //choose a spell
        let spell = choices.[_rand.Next (choices.Length-1)]
        let cost = snd spell
        me.mana <- me.mana - cost

        //apply immediate
        match fst spell with
        | Missile -> boss.hit <- boss.hit - 4
        | Drain -> 
            boss.hit <- boss.hit - 2
            me.hit <- me.hit + 2
        | Shield | Poison -> me.effects <- me.effects |> List.append [(fst spell, 6)]
        | Recharge -> me.effects <- me.effects |> List.append [(fst spell, 5)]

        //apply pending spells for boss turn
        apply me boss

        //boss hits me 
        me.hit <- me.hit - Math.Max(boss.dmg - me.amr, 1)

        if boss.hit <= 0 then mana+cost
        elif me.hit <= 0 then 
            Int32.MaxValue
        else sim spells me boss (mana+cost)

        

[<EntryPoint>]
let main argv = 
    
    let spells = 
        [|(Spell.Missile, 53); (Spell.Drain, 73); 
        (Spell.Shield, 113); (Spell.Poison, 173); (Spell.Recharge, 229);|]


    Seq.initInfinite (fun n -> n)
    |> Seq.map (fun _ -> 
        let me = {hit = 50; mana = 500; amr = 0; effects = List.empty<Spell*int>}
        let boss = {hit = 55; dmg = 8;} 
        sim spells me boss 0
        )
    |> Seq.find (fun mana -> mana < Int32.MaxValue)
    |> printfn "%d"
    
    



    Console.Read ()