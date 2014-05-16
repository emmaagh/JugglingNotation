module InversionTesting

    open System.Collections.Generic
    open Xunit
    open InvertJugglingSequence

    let private testInvertingTwice (s:int list) = Assert.Equal<int list> (s, s |> invert |> invert)

    [<Fact>]
    let ``Test inversion results`` () =
        Assert.Equal<int list> ([5;3;1], invert [5;3;1])
        Assert.Equal<int list> ([3], invert [3])
        Assert.Equal<int list> ([7;7;4;1;1], invert [1;7;7;4;1])
        Assert.Equal<int list> ([4;5;5;6], invert [5;5;6;4])

    [<Fact>]
    let ``Test inverting twice produces original sequence`` () =
        testInvertingTwice [5;3;1]
        testInvertingTwice [3]
        testInvertingTwice [10;2;3]
        testInvertingTwice [3;1;2;2]
        testInvertingTwice [1;7;7;4;1]
        testInvertingTwice [5;5;6;4]