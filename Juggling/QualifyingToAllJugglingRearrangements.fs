module QualifyingToAllJugglingRearrangements

    // Fairly brute force implementation

    open ModularArithmetic
    open BasicJugglingFunctions

    let private listRemoveFirst element =
        let folder (xs, state) x = if state && x = element then xs, not state
                                    else x :: xs, state
        List.fold folder ([], true) >> fst

    let private findAllJugglingRearrangements' inputSequence =
        let (+~) = (ModuloP (List.length inputSequence)).AddModP ()
        let rec helper slotsTaken = function
            | []                -> Set.singleton []
            | inputSequenceTail -> let count = Set.count slotsTaken
                                   inputSequenceTail
                                   |> List.filter (fun a -> slotsTaken |> Set.contains (a +~ count) |> not)
                                   |> Set.ofList
                                   |> Set.map (fun a -> inputSequenceTail
                                                        |> listRemoveFirst a
                                                        |> helper (Set.add (a +~ count) slotsTaken)
                                                        |> Set.map (fun tail -> a::tail))
                                   |> Set.unionMany
        match inputSequence with
        | []      -> Set.empty
        | n::ns   -> ns
                     |> helper (Set.singleton n)
                     |> Set.map (fun tail -> n::tail)

    // Finds all rearrangements of a qualifying sequence to juggling sequences 
    // Remark: Not yet finished / fully tested
    let findAllJugglingRearrangements inputSequence =
        if qualifies inputSequence then
            findAllJugglingRearrangements' inputSequence
        else
            failwith "Input sequence does not qualify"