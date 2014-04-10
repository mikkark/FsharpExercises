namespace Misc

module LotteryNumbers =
    //Create 7 lottery numbers and sort them.

    let rand = new System.Random();

    let rec findDistinctNumber (next:int, l:int list) =
        match List.exists((=) next) l with
        | false -> next
        | true  -> findDistinctNumber (rand.Next(32), l)

    let rec getLotteryNumbers (l:int list) =
        match l.Length with
        | 7    -> l
        | _    -> getLotteryNumbers (findDistinctNumber (rand.Next(32), l) :: l)

    let sortedLotteryNumbers = 
        getLotteryNumbers [] |> List.sort

    printfn "%A" sortedLotteryNumbers