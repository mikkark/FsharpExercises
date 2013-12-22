namespace ReaktorDevDays

module MaxByMultiplyingConsecutiveNumbers =
// When given 500 character length string of numbers,
// find the maximum value when multiplying five consecutive numbers
// (= 34992 (_ numbers-in-string))

    let numbersinstring = "37900490610897696126265185408732594047834333441947" +
                          "01850393807417064181700348379116686008018966949867" +
                          "75587222482716536850061657037580780205386629145841" +
                          "06964490601037178417735301109842904952970798120105" +
                          "47016802197685547844962006690576894353336688823830" +
                          "22913337214734911490555218134123051689058329294117" +
                          "83011983450277211542535458190375258738804563705619" +
                          "55277740874464155295278944953199015261800156422805" +
                          "72771774460964310684699893055144451845092626359982" +
                          "79063901081322647763278370447051079759349248247518"

    let folder acc x = acc*(System.Int32.Parse(x.ToString()))

    let maxSum = 
        List.ofArray(numbersinstring.ToCharArray())
        |> Seq.windowed(5)
        |> Seq.maxBy(Seq.fold folder 1)
        |> Array.fold folder 1

    printfn " maksimi %d" maxSum
    //printfn " maksimi %d" maxSum.Head

//        let take5 (str:string,start:int) =
//        let len = if start + 5 > str.Length then
//                    str.Length - start - 1
//     "    §             else
//                    5 - 1
//        str.[start..start + len]
//this works
    let take5Works (str:string,start:int) =
        let len = if start + 5 > str.Length then
                    str.Length - start - 1
                  else
                    5 - 1
        str.[start..start + len]

    let maxSumWorks = 
         [for i in 0 .. numbersinstring.Length - 6 do
             yield [for char in take5Works(numbersinstring, i) do
                    yield System.Int32.Parse(char.ToString()) ]]
             |> List.map(List.fold (fun acc (x) -> acc*x) 1)
             |> List.max

    printfn " maksimi %d" maxSumWorks
    
//end: this works

//    let multiplyNumbers numbers =
//        List.fold (fun acc (x) -> acc*x) 1 numbers

//    let consecutives = 
//        [ for i in 0 .. numbersinstring.Length - 6 do
//            yield take5(numbersinstring, i)]

//    let asnumbers = [for numberString in consecutives do
//                        yield [for char in numberString do
//                                yield System.Int32.Parse(char.ToString())]]
            
//    let asnumbers = for numberString in consecutives do
//                        numberString
//                        |> List.map(fun i -> System.Int32.Parse(i.ToString()))

//    let sums =
//        consecutiveNumbers
//        |> List.map(multiplyNumbers)

//    let maxSum = 
//        sums
//        |> List.sortBy(fun i -> i)
//        |> List.rev
//        |> List.head