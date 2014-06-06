module QualifyingToAllJugglingRearrangementsTests

    open Xunit
    open BasicJugglingFunctions
    open QualifyingToAllJugglingRearrangements

    let private test input expected =
        let results = findAllJugglingRearrangements input
        Assert.Equal (Set.count expected, Set.count results)
        results
        |> Set.iter (fun s -> expected
                              |> Set.exists (equalUpToCyclicPermutation s)
                              |> Assert.True)

    [<Fact>]
    let ``Test`` () =
        test [3] <| Set.singleton [3]
        test [4;4] <| Set.singleton [4;4]
        test [3;5;4] <| getCyclicPermutations [3;4;5]
        test [3;6;4;3] <| getCyclicPermutations [3;4;6;3]
        test [1;4;5;6] <| Set.union (getCyclicPermutations [1;5;6;4]) (getCyclicPermutations [1;6;4;5])

        [3;4;5;5;5;2;7;9;3;3;8;6;5]
        |> findAllJugglingRearrangements
        |> Set.iter (isJuggleable >> Assert.True)