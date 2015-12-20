// Jeff Jankowski - 12/20/2015
// http://adventofcode.com/day/20

let factors number = seq {
    for divisor in 1 .. (float >> sqrt >> int) number do
    if number % divisor = 0 then
        yield (number, divisor)
        yield (number, number / divisor) }

[<EntryPoint>]
let main argv = 
    let input = 29000000
    let find filt pres = 
        Seq.initInfinite (fun i -> 
            (factors i) 
            |> Seq.distinctBy snd 
            |> Seq.filter filt
            |> Seq.sumBy snd 
            |> (*) pres )
        |> Seq.findIndex (fun sum -> sum >= input)

    printfn "10 Presents:      %d" (find (fun _ -> true) 10)
    printfn "11 and 50 Houses: %d" (find (fun (n,fact) -> n/fact <= 50) 11)

    System.Console.Read () 