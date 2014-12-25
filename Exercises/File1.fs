module rakkaudenFastTrack =

    let names = System.IO.File.ReadLines(@"C:\Omat projektit\Fsharpexercises\Exercises\fast_track_generoitu_nimilista.txt") |> Seq.toList

    let getCountForChar (cP, cA, cI, cR, cS) c =
        match c with
            | 'p'   -> (cP + 1, cA, cI, cR, cS)
            | 'a'   -> (cP, cA + 1, cI, cR, cS)
            | 'i'   -> (cP, cA, cI + 1, cR, cS)
            | 'r'   -> (cP, cA, cI, cR + 1, cS)
            | 's'   -> (cP, cA, cI, cR, cS + 1)
            | _     -> (cP, cA, cI, cR, cS)
    
    let getCharCounts (str:string) =
        str.ToLower().ToCharArray() |> Array.fold getCountForChar (0, 0, 0, 0, 0)

    let sumCharCounts (cP, cA, cI, cR, cS) (cP2, cA2, cI2, cR2, cS2) =
        [cP + cP2; cA + cA2; cI + cI2; cR + cR2; cS + cS2]

    let rec sumUntilLessThan10(listItem:int []) =
        let res = listItem.[0] + listItem.[1]        
        if res < 10 then res else res.ToString().ToCharArray() |> Array.map(fun number -> System.Int32.Parse(number.ToString())) |> sumUntilLessThan10

    let rec calculatePAIRS(counts:int list) =
        match counts.Length with
            | 2 -> System.Int32.Parse(counts.Head.ToString() + counts.Tail.[0].ToString())
            | _ -> counts
                    |> Seq.windowed(2) 
                    |> Seq.toList 
                    |> List.map sumUntilLessThan10
                    |> calculatePAIRS

    let rec matchWithName(names:string list, name:string, res) =
        match names.Length with
            | 0 -> res
            | _ -> let charcounts = sumCharCounts (getCharCounts name) (getCharCounts names.Head)
                   let matchResult = calculatePAIRS charcounts                          
                   if matchResult = 99 then
                        matchWithName(names.Tail, name, List.append res [names.Head + "+" + name]) 
                   else 
                        matchWithName(names.Tail, name, res)

    let rec findMatches(names:string list, res) =
        match names.Tail.Length with
            | 0 -> res
            | _ -> findMatches(names.Tail, matchWithName (names.Tail, names.Head, res))

    let totalNames = names.Length;
        
    let res = findMatches (names, []) |> List.sort
    printfn "%d" res.Length