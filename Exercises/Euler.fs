﻿namespace Euler

module Problem8 =

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

module Problem11 =
    //http://projecteuler.net/problem=11

    //This is a rather 'brute force' solution, but I could not come up with anything more 'F sharpy' :)
    //I first tried applying the same technique as in the problem above (max product of five consecutive numbers)
    //by creating one long consecutive array of numbers (with '00' to mark the end of a row/column/diagonal which then
    //made the product 0) but that was terribly slow.

    let time = System.DateTime.Now

    let rows = ["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08";
                "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00";
                "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65";
                "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91";
                "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80";
                "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50";
                "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70";
                "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21";
                "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72";
                "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95";
                "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92";
                "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57";
                "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58";
                "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40";
                "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66";
                "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69";
                "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36";
                "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16";
                "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54";
                "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48";]
       
    let splitRows =
        rows |> List.map (fun line -> line.Split[|' '|] |> Array.map (fun strNumber -> System.Int32.Parse(strNumber.ToString())))

    let horizontalProd x y =
        splitRows.[y].[x] * splitRows.[y].[x + 1 ] * splitRows.[y].[x + 2] * splitRows.[y].[x + 3]

    let verticalProd x y =
        splitRows.[y].[x] * splitRows.[y + 1].[x] * splitRows.[y + 2].[x] * splitRows.[y + 3].[x]

    let diagonalProd1 x y =
        splitRows.[y].[x] * splitRows.[y + 1].[x + 1] * splitRows.[y + 2].[x + 2] * splitRows.[y + 3].[x + 3]

    let diagonalProd2 x y =
        splitRows.[y].[x] * splitRows.[y + 1].[x - 1] * splitRows.[y + 2].[x - 2] * splitRows.[y + 3].[x - 3]

    let findMax x y =       
        let arr = 
            [| if x <= 16 then yield horizontalProd x y
               if y <= 16 then yield verticalProd x y
               if x <= 16 && y <= 16 then yield diagonalProd1 x y
               if x >= 3 && y <= 16 then yield diagonalProd2 x y |]
        if arr.Length > 0 then
            Array.max(arr)
        else
            0

    let maximums =
         [ for x in 0..19 do
            for y in 0..19 do
                yield findMax x y ]

    printfn "maksimi on %d" ( List.max(maximums) )
    printfn "aikaa kului %f" (System.DateTime.Now.Subtract(time).TotalMilliseconds)
   
module Problem59 =
    //http://projecteuler.net/problem=59

    let crackthis = "79,59,12,2,79,35,8,28,20,2,3,68,8,9,68,45,0,12,9,67,68,4,7,5,23,27,1,21,79," +
                    "85,78,79,85,71,38,10,71,27,12,2,79,6,2,8,13,9,1,13,9,8,68,19,7,1,71,56,11,21," +
                    "11,68,6,3,22,2,14,0,30,79,1,31,6,23,19,10,0,73,79,44,2,79,19,6,28,68,16,6,16,15,"+
                    "79,35,8,11,72,71,14,10,3,79,12,2,79,19,6,28,68,32,0,0,73,79,86,71,39,1,71,24,5,20,"+
                    "79,13,9,79,16,15,10,68,5,10,3,14,1,10,14,1,3,71,24,13,19,7,68,32,0,0,73,79,87,71,"+ 
                    "39,1,71,12,22,2,14,16,2,11,68,2,25,1,21,22,16,15,6,10,0,79,16,15,10,22,2,79,13,20,"+
                    "65,68,41,0,16,15,6,10,0,79,1,31,6,23,19,28,68,19,7,5,19,79,12,2,79,0,14,11,10,64,27,"+
                    "68,10,14,15,2,65,68,83,79,40,14,9,1,71,6,16,20,10,8,1,79,19,6,28,68,14,1,68,15,6,9,75,79,5,9,11,68,19,7,13,20,79,8,14,9,1,71,8,13,17,10,23,71,3,13,0,7,16,71,27,11,71,10,18,2,29,29,8,1,1,73,79,81,71,59,12,2,79,8,14,8,12,19,79,23,15,6,10,2,28,68,19,7,22,8,26,3,15,79,16,15,10,68,3,14,22,12,1,1,20,28,72,71,14,10,3,79,16,15,10,68,3,14,22,12,1,1,20,28,68,4,14,10,71,1,1,17,10,22,71,10,28,19,6,10,0,26,13,20,7,68,14,27,74,71,89,68,32,0,0,71,28,1,9,27,68,45,0,12,9,79,16,15,10,68,37,14,20,19,6,23,19,79,83,71,27,11,71,27,1,11,3,68,2,25,1,21,22,11,9,10,68,6,13,11,18,27,68,19,7,1,71,3,13,0,7,16,71,28,11,71,27,12,6,27,68,2,25,1,21,22,11,9,10,68,10,6,3,15,27,68,5,10,8,14,10,18,2,79,6,2,12,5,18,28,1,71,0,2,71,7,13,20,79,16,2,28,16,14,2,11,9,22,74,71,87,68,45,0,12,9,79,12,14,2,23,2,3,2,71,24,5,20,79,10,8,27,68,19,7,1,71,3,13,0,7,16,92,79,12,2,79,19,6,28,68,8,1,8,30,79,5,71,24,13,19,1,1,20,28,68,19,0,68,19,7,1,71,3,13,0,7,16,73,79,93,71,59,12,2,79,11,9,10,68,16,7,11,71,6,23,71,27,12,2,79,16,21,26,1,71,3,13,0,7,16,75,79,19,15,0,68,0,6,18,2,28,68,11,6,3,15,27,68,19,0,68,2,25,1,21,22,11,9,10,72,71,24,5,20,79,3,8,6,10,0,79,16,8,79,7,8,2,1,71,6,10,19,0,68,19,7,1,71,24,11,21,3,0,73,79,85,87,79,38,18,27,68,6,3,16,15,0,17,0,7,68,19,7,1,71,24,11,21,3,0,71,24,5,20,79,9,6,11,1,71,27,12,21,0,17,0,7,68,15,6,9,75,79,16,15,10,68,16,0,22,11,11,68,3,6,0,9,72,16,71,29,1,4,0,3,9,6,30,2,79,12,14,2,68,16,7,1,9,79,12,2,79,7,6,2,1,73,79,85,86,79,33,17,10,10,71,6,10,71,7,13,20,79,11,16,1,68,11,14,10,3,79,5,9,11,68,6,2,11,9,8,68,15,6,23,71,0,19,9,79,20,2,0,20,11,10,72,71,7,1,71,24,5,20,79,10,8,27,68,6,12,7,2,31,16,2,11,74,71,94,86,71,45,17,19,79,16,8,79,5,11,3,68,16,7,11,71,13,1,11,6,1,17,10,0,71,7,13,10,79,5,9,11,68,6,12,7,2,31,16,2,11,68,15,6,9,75,79,12,2,79,3,6,25,1,71,27,12,2,79,22,14,8,12,19,79,16,8,79,6,2,12,11,10,10,68,4,7,13,11,11,22,2,1,68,8,9,68,32,0,0,73,79,85,84,79,48,15,10,29,71,14,22,2,79,22,2,13,11,21,1,69,71,59,12,14,28,68,14,28,68,9,0,16,71,14,68,23,7,29,20,6,7,6,3,68,5,6,22,19,7,68,21,10,23,18,3,16,14,1,3,71,9,22,8,2,68,15,26,9,6,1,68,23,14,23,20,6,11,9,79,11,21,79,20,11,14,10,75,79,16,15,6,23,71,29,1,5,6,22,19,7,68,4,0,9,2,28,68,1,29,11,10,79,35,8,11,74,86,91,68,52,0,68,19,7,1,71,56,11,21,11,68,5,10,7,6,2,1,71,7,17,10,14,10,71,14,10,3,79,8,14,25,1,3,79,12,2,29,1,71,0,10,71,10,5,21,27,12,71,14,9,8,1,3,71,26,23,73,79,44,2,79,19,6,28,68,1,26,8,11,79,11,1,79,17,9,9,5,14,3,13,9,8,68,11,0,18,2,79,5,9,11,68,1,14,13,19,7,2,18,3,10,2,28,23,73,79,37,9,11,68,16,10,68,15,14,18,2,79,23,2,10,10,71,7,13,20,79,3,11,0,22,30,67,68,19,7,1,71,8,8,8,29,29,71,0,2,71,27,12,2,79,11,9,3,29,71,60,11,9,79,11,1,79,16,15,10,68,33,14,16,15,10,22,73"
    
    let chars = [97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122]
    
    let decode3 (charlist:int list, key:int list) =
        List.map2(fun x y -> char <| x ^^^ y) charlist key
        |> List.fold (fun acc (c) -> acc + c.ToString()) ""
        
    let rec decode (charlist:int list, key:int list, acc:string) =
        match charlist with
        | h::t -> if t.Length >= 2 then decode (Seq.skip 2 t |> Seq.toList, key, acc + decode3(h :: [for i in 0..1 do yield t.Item(i)], key)) else acc
        | []   -> acc

    let time = System.DateTime.Now
    
    let bruteforce (tocrack:string) = for c in chars do
                                         for c1 in chars do
                                            for c2 in chars do
                                                let text = decode(List.ofArray(tocrack.Split(',')) |> List.map(fun s -> System.Int32.Parse(s.ToString())), [c;c1;c2], "")
                                                if text.Contains(" and ") then
                                                    let key = [c;c1;c2] |> List.fold(fun acc (c) -> acc + c.ToString()) ""
                                                    printfn "avain on %s" key
                                                    printfn "teksti on %s" text
                                                    printfn "aikaa kului %f" (System.DateTime.Now.Subtract(time).TotalMilliseconds)

module firstVersion =

    let crackthis = "79,59,12,2,79,35,8,28,20,2,3,68,8,9,68,45,0,12,9,67,68,4,7,5,23,27,1,21,79," +
                    "85,78,79,85,71,38,10,71,27,12,2,79,6,2,8,13,9,1,13,9,8,68,19,7,1,71,56,11,21," +
                    "11,68,6,3,22,2,14,0,30,79,1,31,6,23,19,10,0,73,79,44,2,79,19,6,28,68,16,6,16,15,"+
                    "79,35,8,11,72,71,14,10,3,79,12,2,79,19,6,28,68,32,0,0,73,79,86,71,39,1,71,24,5,20,"+
                    "79,13,9,79,16,15,10,68,5,10,3,14,1,10,14,1,3,71,24,13,19,7,68,32,0,0,73,79,87,71,"+ 
                    "39,1,71,12,22,2,14,16,2,11,68,2,25,1,21,22,16,15,6,10,0,79,16,15,10,22,2,79,13,20,"+
                    "65,68,41,0,16,15,6,10,0,79,1,31,6,23,19,28,68,19,7,5,19,79,12,2,79,0,14,11,10,64,27,"+
                    "68,10,14,15,2,65,68,83,79,40,14,9,1,71,6,16,20,10,8,1,79,19,6,28,68,14,1,68,15,6,9,75,79,5,9,11,68,19,7,13,20,79,8,14,9,1,71,8,13,17,10,23,71,3,13,0,7,16,71,27,11,71,10,18,2,29,29,8,1,1,73,79,81,71,59,12,2,79,8,14,8,12,19,79,23,15,6,10,2,28,68,19,7,22,8,26,3,15,79,16,15,10,68,3,14,22,12,1,1,20,28,72,71,14,10,3,79,16,15,10,68,3,14,22,12,1,1,20,28,68,4,14,10,71,1,1,17,10,22,71,10,28,19,6,10,0,26,13,20,7,68,14,27,74,71,89,68,32,0,0,71,28,1,9,27,68,45,0,12,9,79,16,15,10,68,37,14,20,19,6,23,19,79,83,71,27,11,71,27,1,11,3,68,2,25,1,21,22,11,9,10,68,6,13,11,18,27,68,19,7,1,71,3,13,0,7,16,71,28,11,71,27,12,6,27,68,2,25,1,21,22,11,9,10,68,10,6,3,15,27,68,5,10,8,14,10,18,2,79,6,2,12,5,18,28,1,71,0,2,71,7,13,20,79,16,2,28,16,14,2,11,9,22,74,71,87,68,45,0,12,9,79,12,14,2,23,2,3,2,71,24,5,20,79,10,8,27,68,19,7,1,71,3,13,0,7,16,92,79,12,2,79,19,6,28,68,8,1,8,30,79,5,71,24,13,19,1,1,20,28,68,19,0,68,19,7,1,71,3,13,0,7,16,73,79,93,71,59,12,2,79,11,9,10,68,16,7,11,71,6,23,71,27,12,2,79,16,21,26,1,71,3,13,0,7,16,75,79,19,15,0,68,0,6,18,2,28,68,11,6,3,15,27,68,19,0,68,2,25,1,21,22,11,9,10,72,71,24,5,20,79,3,8,6,10,0,79,16,8,79,7,8,2,1,71,6,10,19,0,68,19,7,1,71,24,11,21,3,0,73,79,85,87,79,38,18,27,68,6,3,16,15,0,17,0,7,68,19,7,1,71,24,11,21,3,0,71,24,5,20,79,9,6,11,1,71,27,12,21,0,17,0,7,68,15,6,9,75,79,16,15,10,68,16,0,22,11,11,68,3,6,0,9,72,16,71,29,1,4,0,3,9,6,30,2,79,12,14,2,68,16,7,1,9,79,12,2,79,7,6,2,1,73,79,85,86,79,33,17,10,10,71,6,10,71,7,13,20,79,11,16,1,68,11,14,10,3,79,5,9,11,68,6,2,11,9,8,68,15,6,23,71,0,19,9,79,20,2,0,20,11,10,72,71,7,1,71,24,5,20,79,10,8,27,68,6,12,7,2,31,16,2,11,74,71,94,86,71,45,17,19,79,16,8,79,5,11,3,68,16,7,11,71,13,1,11,6,1,17,10,0,71,7,13,10,79,5,9,11,68,6,12,7,2,31,16,2,11,68,15,6,9,75,79,12,2,79,3,6,25,1,71,27,12,2,79,22,14,8,12,19,79,16,8,79,6,2,12,11,10,10,68,4,7,13,11,11,22,2,1,68,8,9,68,32,0,0,73,79,85,84,79,48,15,10,29,71,14,22,2,79,22,2,13,11,21,1,69,71,59,12,14,28,68,14,28,68,9,0,16,71,14,68,23,7,29,20,6,7,6,3,68,5,6,22,19,7,68,21,10,23,18,3,16,14,1,3,71,9,22,8,2,68,15,26,9,6,1,68,23,14,23,20,6,11,9,79,11,21,79,20,11,14,10,75,79,16,15,6,23,71,29,1,5,6,22,19,7,68,4,0,9,2,28,68,1,29,11,10,79,35,8,11,74,86,91,68,52,0,68,19,7,1,71,56,11,21,11,68,5,10,7,6,2,1,71,7,17,10,14,10,71,14,10,3,79,8,14,25,1,3,79,12,2,29,1,71,0,10,71,10,5,21,27,12,71,14,9,8,1,3,71,26,23,73,79,44,2,79,19,6,28,68,1,26,8,11,79,11,1,79,17,9,9,5,14,3,13,9,8,68,11,0,18,2,79,5,9,11,68,1,14,13,19,7,2,18,3,10,2,28,23,73,79,37,9,11,68,16,10,68,15,14,18,2,79,23,2,10,10,71,7,13,20,79,3,11,0,22,30,67,68,19,7,1,71,8,8,8,29,29,71,0,2,71,27,12,2,79,11,9,3,29,71,60,11,9,79,11,1,79,16,15,10,68,33,14,16,15,10,22,73"
    
    let time = System.DateTime.Now
    let crackthislist = List.ofArray(crackthis.Split(','))
        
    let bruteforce = for c in [| int 'a'..int 'z'|] do
                        for c1 in [| int 'a'..int 'z'|] do
                            for c2 in [| int 'a'..int 'z'|] do
                                let chars = [| c; c1; c2|]
                                let mutable accumulator = (0, "")
                                accumulator <- crackthislist 
                                            |> List.fold(fun acc (c) -> (fst acc + 1, (snd acc) + (char (int c ^^^ chars.[(fst acc)%3])).ToString())) accumulator
                                if (snd accumulator).Contains(" and ") then
                                    let key = [c;c1;c2] |> List.fold(fun acc (c) -> acc + (char c).ToString()) ""
                                    printfn "key is %s" key
                                    printfn "text is %s" (snd accumulator)
                                    printfn "time spent %f" (System.DateTime.Now.Subtract(time).TotalMilliseconds)
                                    
    printfn "finished"
//    let first = arraysOfThree.Head
//    let length = snd first |> Seq.toList |> List.length
//
//    printfn "items count %d" length

    //ota käsittelyyn kaikki merkit
    //kokeile niihin kolmen merkin avainyhdistelmää
    //etsi muodostuvista stringeistä vaikka " and " tai " but " merkkijonoja