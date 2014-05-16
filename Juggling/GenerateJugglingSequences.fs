module GenerateJugglingSequences

    open GeneratePermutations
    open ModularArithmetic
    open BasicJugglingFunctions

    let private computePhi' p =
        let (-~) = (ModuloP p).SubtractModP ()
        List.mapi (fun i ai -> ai -~ i)

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
        let computePhi' = computePhi' p
        generatePermutations p
        |> Set.map computePhi'
        |> Set.map (fun phi' -> phi', (b - (balls phi')))
        |> Set.map (fun (phi', b') -> phi', generateNonNegativeLists b' p)
        |> Set.map (fun (phi', phi''s) -> Set.map (fun phi'' -> List.map2 (fun x y -> x + p*y) phi' phi'') phi''s)
        |> Set.unionMany