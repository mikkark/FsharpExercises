namespace pahkinat

module pahkinaSlowAndGood = 

    type LinkStation(name, location, reach) =
        member this.Name
            with get() = name
        
        member this.X
            with get() = fst location

        member this.Y
            with get() = snd location

        member this.Reach
            with get() = reach

    type Device(location) =
        member this.X
            with get() = fst location

        member this.Y
            with get() = snd location

        member this.GiveCenteredLocation relativeTo =
             abs(this.X - fst relativeTo), abs(this.Y - snd relativeTo)
    
    type MostSuitableStation = {
        Station: LinkStation
        Power: float
    }

    let linkStations = [
                        new LinkStation("Helsinki", (0,0), 10.0);
                        new LinkStation("Porvoo", (20,20), 5.0);
                        new LinkStation("Kajaani", (10,0), 12.0)
                       ]

    let calculateDistance (x:int, y:int) =
        sqrt (((float)x ** 2.0) + ((float)y ** 2.0))
        
    let calculatePower (linkStation:LinkStation, distance:float) =
        if distance > linkStation.Reach then 0.0 else (linkStation.Reach - distance) ** 2.0

    let getSuitableStation (device:Device) =
        linkStations |> List.map (fun item -> (item, calculateDistance (device.GiveCenteredLocation(item.X,item.Y)) ))
                     |> List.fold (fun acc item -> 
                                        let power = calculatePower (fst item, snd item)
                                        match power > acc.Power with
                                            | true  -> { Station = fst item; Power = power }
                                            | false -> acc
                                        ) { Station = linkStations.Head; Power = 0.0 }

    printfn "%A" (getSuitableStation (new Device (0, 0))).Station.Name
    printfn "%A" (getSuitableStation (new Device (100, 100))).Station.Name
    printfn "%A" (getSuitableStation (new Device (15, 10))).Station.Name
    printfn "%A" (getSuitableStation (new Device (18, 18))).Station.Name

module pahkinaQuickAndDirty = 

    let linkStations = [
                        [0; 0; 10];
                        [20; 20; 5];
                        [10; 0; 12]
                        ]

    let calculateDistance (x:int, y:int) =
        sqrt (((float)x ** 2.0) + ((float)y ** 2.0))
        
    let calculatePower (reach:float, distance:float) =
        if distance > reach then 0.0 else (reach - distance) ** 2.0

    let getSuitableStation (device:int*int) =
        let distances = linkStations |> List.map (fun item -> (item.[2], calculateDistance (abs(fst device - item.[0]), abs(snd device - item.[1]))))
        distances |> List.map (fun item -> calculatePower ((float)(fst item), snd item)) |> List.sort |> List.rev |> List.head

    printfn "%A" (getSuitableStation (0, 0))
    printfn "%A" (getSuitableStation (100, 100))
    printfn "%A" (getSuitableStation (15, 10))
    printfn "%A" (getSuitableStation (18, 18))