module BasicJugglingFunctionsTests

    open Xunit
    open BasicJugglingFunctions

    [<Fact>]
    let testBallLandingsWithinInterval () =
        let pattern = [5;3;1]
        let start, ``end`` = 2,8
        let landings = getBallLandingsWithinInterval pattern start ``end``
        Assert.True (List.length landings = 3)
        landings
        |> List.exists ((=) (Set.ofList [2;3;8]))
        |> Assert.True
        landings
        |> List.exists ((=) (Set.ofList [4;7]))
        |> Assert.True
        landings
        |> List.exists ((=) (Set.ofList [5;6]))
        |> Assert.True