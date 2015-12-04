// Jeff Jankowski - 12/04/2015
// http://adventofcode.com/day/4


open System.Security.Cryptography

let rec hash (md5 : MD5) root n (target : string)=
    let bytes = md5.ComputeHash( System.Text.Encoding.UTF8.GetBytes(root + n.ToString()))
    let str = System.BitConverter.ToString(bytes).Replace("-", "")
    match str.[0..target.Length-1] with
        | chop when chop = target -> (root + n.ToString(), str)
        | _ -> hash md5 root (n + 1) target


[<EntryPoint>]
let main argv = 
    let md5 = MD5.Create()
    let find = hash md5 "yzbqklnj" 1
    let print = printfn "Key:  %s \nHash: %s\n"

    let five = find "00000"
    print (fst five) (snd five)
    let six = find "000000"
    print (fst six) (snd six)

    System.Console.Read();
