module GenerateJugglingSequences

    open GeneratePermutations
    open ModularArithmetic
    open BasicJugglingFunctions

    let private computeTestVector' p =
        mapToValuesModP
        >> List.mapi (fun i ai -> (ai -. i).value)

    let rec private generateNonNegativeLists sum length =
        match sum, length with
        | 0, 0              -> Set.singleton []
        | _ when sum < 0    -> Set.empty
        | _, 0              -> Set.empty
        | _, _              -> List.init (sum+1) id
                              |> List.map (fun head -> generateNonNegativeLists (sum-head) (length-1)
                                                       |> Set.map (fun tail -> head::tail))
                              |> Set.unionMany

    // Generates all of the juggling sequences of period p and balls b
    let generateJugglingSequences b p =
        generatePermutations p
        |> Set.map (computeTestVector' p)
        |> Set.map (fun testVector' -> testVector', b - balls testVector')
        |> Set.map (fun (testVector', b') -> testVector', generateNonNegativeLists b' p)
        |> Set.map (fun (testVector', qs) -> qs |> Set.map (List.map2 (fun x y -> x + p*y) testVector'))
        |> Set.unionMany