// Basic juggling related functions on sequences
module BasicJugglingFunctions

    open ModularArithmetic
    
    let period = List.length
    
    let mapToValuesModP s = s |> List.map (fun a -> { period = period s; v = a })

    let isNonNegative = List.forall ((<=) 0)

    let hasIntegerAverage s =
        List.sum s
        |> modulo (period s)
        |> ((=) 0)

    let height = List.max

    let jugglingFunction s =
        modulo <| period s
        >> List.nth s

    let phiOnSequenceInterval =
        mapToValuesModP
        >> List.mapi (+.)
        >> List.map (fun m -> m.value)
    
    let phi s i = i + jugglingFunction s i

    let private phiIsJugglingFuction s =
        phiOnSequenceInterval s
        |> List.sort
        |> List.forall2 (=) (List.init (period s) id)

    let qualifies s =
        isNonNegative s && hasIntegerAverage s

    let isJuggleable s = isNonNegative s && phiIsJugglingFuction s

    let (|EmptySequence|JugglingSequence|QualifyingSequence|NonQualifyingSequence|) = function
        | []  -> EmptySequence
        | s   -> if      isJuggleable s then JugglingSequence
                 else if qualifies s    then QualifyingSequence
                 else                        NonQualifyingSequence

    let balls s =
        match s with
        | JugglingSequence  -> int <| List.averageBy float s
        | _                 -> failwith <| sprintf "%A is not a juggling sequence" s

    let cyclicPermutation s n =
        s
        |> List.permute ((+) n >> (modulo <| period s))

    let getCyclicPermutations s =
        cyclicPermutation s
        |> List.init (period s)
        |> Set.ofList

    let equalUpToCyclicPermutation s1 s2 =
        getCyclicPermutations s1
        |> Set.exists ((=) s2)

    // The classes of ball landings within the interval (inclusive of endpoints)
    let getBallLandingsWithinInterval s start ``end`` =
        let length = ``end`` - start + 1
        // TODO: Make algorithm work without this restriction
        if length < height s then failwith "Interval too small to work over - length must be at least the height of the sequence (as algorithm needs to guarantee every ball lands in the interval)"
        let getLandings =
            let rec helper landings i =
                if i > ``end`` then
                    landings
                else
                    helper <| Set.add i landings <| phi s i
            helper Set.empty
        let interval = List.init length ((+) start)
        let rec getClasses classes =
            match interval |> List.filter (fun i -> classes |> Set.unionMany |> Set.contains i |> not && jugglingFunction s i <> 0) with
            | []    -> classes
            | i::_  -> getLandings i :: classes |> getClasses
        getClasses []

    let indexOfSender s i =
        let rec helper j =
            if phi s j = i then j
            else helper <| j-1
        helper i