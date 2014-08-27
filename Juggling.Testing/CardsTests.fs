module CardsTests

    open Xunit
    open Cards

    let siteswapCardsPairs = [
        ([3;3;3], [3;3;3]);
        ([7;1;1], [1;3;1]);
        ([5;3;1], [1;2;3]);
        ([5;0;4], [3;0;3]);
        ([4;4;1], [1;3;3]);
        ([7;5;3;1], [1;2;3;4]);
        ([4;5;1;4;1], [1;3;3;1;3])
    ]

    [<Fact>]
    let testConvertingSiteswapToCards () =
        let testPair (siteswap, expectedCards) =
            Assert.Equal<int list> (expectedCards, convertSiteswapToCards siteswap)
        siteswapCardsPairs
        |> List.iter testPair

    [<Fact>]
    let testConvertingCardsToSiteswap () =
        let testPair (expectedSiteswap, cards) =
            Assert.Equal<int list> (expectedSiteswap, convertCardsToSiteswap cards)
        siteswapCardsPairs
        |> List.iter testPair