// Jeff Jankowski - 12/23/2015
// http://adventofcode.com/day/23

open System
type Instr = 
    | JIO of string*int
    | INC of string
    | TPL of string
    | JMP of int
    | JIE of string*int
    | HLF of string

let run (ops: Instr[]) a = 
    let mutable ctr = 0;
    let a = ref a;
    let b = ref 0u;
    let reg nm =
        match nm with 
        | "a" -> a
        | "b" -> b
    let rval nm = (reg nm).Value
    while ctr < ops.Length do
        let op = ops.[ctr]
        ctr <-
            match op with
            | JIO(nm,off) -> if rval nm = 1u then ctr + off else ctr+1
            | INC(nm) -> 
                (reg nm) := (rval nm) + 1u
                ctr+1
            | TPL(nm) ->
                (reg nm) := (rval nm) * 3u
                ctr+1
            | JMP(off) -> ctr + off
            | JIE(nm,off) -> if rval nm % 2u = 0u then ctr + off else ctr+1
            | HLF(nm) -> 
                (reg nm) := (rval nm) / 2u
                ctr+1
    b.Value

[<EntryPoint>]
let main argv = 
    let ops =
        IO.File.ReadAllLines "..\..\input.txt"
        |> Array.map (fun str -> 
            let split = str.Split ' '
            match split.[0] with
            | "jio" -> Instr.JIO(split.[1].TrimEnd ',', Int32.Parse split.[2])
            | "inc" -> Instr.INC(split.[1])
            | "tpl" -> Instr.TPL(split.[1])
            | "jmp" -> Instr.JMP(Int32.Parse split.[1])
            | "jie" -> Instr.JIE(split.[1].TrimEnd ',', Int32.Parse split.[2])
            | "hlf" -> Instr.HLF(split.[1]) )

    printfn "Start a:0, b: %d" (run ops 0u)
    printfn "Start a:1, b: %d" (run ops 1u)


    Console.Read ()
