//namespace RosettaCode

module StableMarriageProblem =
    //http://rosettacode.org/wiki/Stable_marriage_problem

    let proposers = 
        ["abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay";
         "bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay";
         "col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan";
         "dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi";
         "ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay";
         "fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay";
         "gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay";
         "hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee";
         "ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve";
         "jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope"]
    
    let proposees =
     ["abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal";
  "bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal";
 "cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon";
  "dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed";
  "eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob";
  "fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal";
  "gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian";
 "hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred";
  "ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan";
  "jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan"]

    let splitter (s:string) = 
        (s.Split(':').[0], 
         s.Split(':').[1].Split(',') |> Array.map(fun preference -> preference.Trim()))

    let proposersWithPreferences = [for proposerLine in proposers do yield splitter proposerLine]
    let proposeesWithPreferences = 
        [for proposeeLine in proposees do 
            yield splitter proposeeLine]
   
    let findProposee proposerName preferedProposee matches =
        let proposee = match List.tryFind(fun elem -> fst elem = preferedProposee) proposeesWithPreferences with
                       | Some x -> x
        let indexOfProposer = match Array.tryFindIndex(fun elem2 -> elem2 = proposerName) (snd proposee) with
                              | Some y -> y
        let isProposeeAlreadyMatched = match List.tryFind(fun elem -> fst elem = fst proposee) matches with
                                       | None   -> None
                                       | Some existingMatch -> Array.tryFindIndex(fun pref -> pref = snd existingMatch) (snd proposee)
        match isProposeeAlreadyMatched with
        | None                      -> printfn "%s becomes paired with %s" proposerName (fst proposee)
                                       (fst proposee, proposerName) :: matches
        | Some indexOfExistingMatch -> if indexOfExistingMatch > indexOfProposer then
                                            (matches |> List.map(fun elem -> if fst elem = fst proposee then (fst proposee, proposerName) else elem)) 
                                       else 
                                            matches
    
    let res =
        let mutable matches = []
        let mutable misses = proposersWithPreferences |> List.map (fun proposer -> (fst proposer, 0))
        let mutable round = 0
        while matches.Length < 10 do
            printfn "Round %d" round
            for proposer in proposersWithPreferences do
                let isProposerMatched = match List.tryFind(fun elem -> snd elem = fst proposer) matches with
                                        | None   -> None
                                        | Some x -> Some x
                match isProposerMatched with
                | None -> printfn "%s prefers %s" (fst proposer) (snd proposer).[snd (misses |> List.find(fun prop -> fst prop = fst proposer))]
                          matches <- findProposee (fst proposer) (snd proposer).[snd (misses |> List.find(fun prop -> fst prop = fst proposer))] matches
                | _    -> matches <- matches
                match List.tryFind(fun elem -> snd elem = fst proposer) matches with
                | None   -> misses <- List.map(fun misser -> if fst misser = fst proposer then (fst misser, snd misser + 1) else misser) misses
                | Some x -> misses <- misses
            round <- round + 1
        matches

    printfn "%A" res

    System.Console.ReadLine