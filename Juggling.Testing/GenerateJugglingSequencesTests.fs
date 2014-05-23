module JugglingSequenceGenerationTests

    open Xunit
    open BasicJugglingFunctions
    open GenerateJugglingSequences

    let private crossProduct set =
        Set.map (fun b -> Set.map (fun a -> a,b) set)
        >> Set.unionMany

    let private testSetEquality setX setY =
        setX
        |> Set.difference setY
        |> Assert.Empty
        setY
        |> Set.difference setX
        |> Assert.Empty

    [<Fact>]
    let ``Test n ball sequences of period m`` () =
        let ballValues = Set.ofList <| List.init 4 ((+) 2)
        let periodValues = Set.ofList <| List.init 4 ((+) 2)
        crossProduct ballValues periodValues
        |> Set.map (fun (b, p) -> generateJugglingSequences b p)
        |> Set.unionMany
        |> Set.iter (isJuggleable >> Assert.True)

    [<Fact>]
    let ``Check 3 ball sequences of period 3 exactly`` () =
        [[9;0;0]; [0;9;0]; [0;0;9]; [0;3;6]; [0;6;3]; [3;0;6]; [3;6;0]; [6;0;3]; [6;3;0]; [3;3;3]; [6;1;2]; [0;7;2]; [0;1;8]; [3;4;2]; [3;1;5]; [0;4;5]; [7;2;0]; [1;8;0]; [1;2;6]; [4;5;0]; [4;2;3]; [1;5;3]; [7;1;1]; [1;7;1]; [1;1;7]; [4;4;1]; [4;1;4]; [1;4;4]; [5;2;2]; [2;5;2]; [2;2;5]; [8;0;1]; [2;6;1]; [2;0;7]; [5;3;1]; [5;0;4]; [2;3;4]]
        |> Set.ofList
        |> testSetEquality (generateJugglingSequences 3 3)