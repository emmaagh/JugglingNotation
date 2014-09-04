module QualifyingToAllJugglingRearrangements

    // Fairly brute force implementation
    // Complexity estimate:
    //     For length of slotsTaken=n and length of inputSequenceTail=m, the complexity of helper is
    //     c(n,m) = n                       (Set.count)
    //              + m.n                   (List.filter)
    //              + m(m + c(n+1,m-1))     (Set.map - very much an upper bound)
    //     Therefore total complexity
    //       = c
    //       = c(0, m)
    //       = m(m + c(1, m-1))
    //       = m(2m + (m-1)(m - 1 + c(2,m-2)))
    //       = m(2m + (m-1)(m - 1 + 2 + 2(m-2) + (m-2)(m - 2 + c(3,m-3))))
    //       = m(2m + (m-1)(3m - 3 + (m-2)(m - 2 + c(3,m-3))))
    //       = O(m!)

    open ModularArithmetic
    open BasicJugglingFunctions

    let private listRemoveFirst element =
        let folder (xs, state) x = if state && x = element then xs, not state
                                   else x :: xs, state
        List.fold folder ([], true) >> fst

    let private findAllJugglingRearrangements' inputSequence =
        let landingWithinInterval = modulo <| period inputSequence
        let rec helper slotsTaken = function
            | []                -> Set.singleton []
            | inputSequenceTail -> let count = Set.count slotsTaken
                                   inputSequenceTail
                                   |> List.filter (fun a -> slotsTaken |> Set.contains (landingWithinInterval <| count + a) |> not)
                                   |> Set.ofList
                                   |> Set.map (fun a -> inputSequenceTail
                                                        |> listRemoveFirst a
                                                        |> helper (Set.add (landingWithinInterval <| count + a) slotsTaken)
                                                        |> Set.map (fun tail -> a::tail))
                                   |> Set.unionMany
        inputSequence
        |> helper Set.empty

    // Finds all rearrangements of a qualifying sequence to juggling sequences 
    let findAllJugglingRearrangements inputSequence =
        if List.isEmpty inputSequence || not <| qualifies inputSequence then
            Set.empty
        else
            findAllJugglingRearrangements' inputSequence