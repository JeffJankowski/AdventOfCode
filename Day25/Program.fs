// Jeff Jankowski - 12/25/2015
// http://adventofcode.com/day/25

let next code = 
    (code * 252533L) % 33554393L

let rec step target (r,c) code = 
        if (r,c) = target then code
        else step target (if r = 1 then (c+1, 1) else (r-1, c+1)) (next code)

[<EntryPoint>]
let main argv = 
    printfn "Code: %d" (step (2978, 3083) (1,1) 20151125L)

    System.Console.Read ()