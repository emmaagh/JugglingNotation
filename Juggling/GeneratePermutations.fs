module GeneratePermutations

    let rec private insertions x = function
        | []                  -> Set.singleton [x]
        | head::tail as perm  -> insertions x tail
                                 |> Set.map (fun insertion -> head::insertion)
                                 |> Set.add (x::perm)

    let rec private permutations input =
        if Set.isEmpty input then
            Set.singleton []
        else
            let element = input |> Set.toList |> List.head
            Set.remove element input
            |> permutations
            |> Set.map (insertions element)
            |> Set.unionMany

    let generatePermutations length =
        List.init length id
        |> Set.ofList
        |> permutations