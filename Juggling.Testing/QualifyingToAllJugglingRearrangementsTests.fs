module QualifyingToAllJugglingRearrangementsTests

    open Xunit
    open BasicJugglingFunctions
    open QualifyingToAllJugglingRearrangements

    [<Fact>]
    let ``Test simple sequences for starters`` () =
        let s = Set.ofList [3;4;5;5;5;2]
        let jugglingSequences = findAllJugglingRearrangements [3;4;5;5;5;2;7;9;3;3;8;6;5]
        printfn "Sequence count: %i" <| Set.count jugglingSequences
        jugglingSequences |> Set.iter (printfn "Juggling sequence: %A")
        jugglingSequences |> Set.iter (isJuggleable >> Assert.True)
        ()