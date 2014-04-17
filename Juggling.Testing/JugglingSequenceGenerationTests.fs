module JugglingSequenceGenerationTests

    open Xunit
    open BasicJugglingFunctions
    open JugglingSequenceGeneration

    let private crossProduct set =
        Set.map (fun b -> Set.map (fun a -> a,b) set)
        >> Set.unionMany

    [<Fact>]
    let ``Test n ball sequences of period m`` () =
        let ballses = Set.ofList <| List.init 4 ((+) 2)
        let periods = Set.ofList <| List.init 4 ((+) 2)
        crossProduct ballses periods
        |> Set.map (fun (b, p) -> generateJugglingSequences b p)
        |> Set.unionMany
        |> Set.iter (isJuggleable >> Assert.True)

    // TODO: Test to check length / input of results.