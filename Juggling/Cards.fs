// Conversion between siteswap notation and juggling card notation
module Cards
    
    open BasicJugglingFunctions
    open ModularArithmetic

    let private withinInterval start ``end`` value = start < value && value <= ``end``

    let convertSiteswapToCards s =
        let ballLandings = getBallLandingsWithinInterval s (- height s) (height s - 1)
        let cardSelector i =
            ballLandings
            |> List.filter (Set.exists <| withinInterval (indexOfSender s i) i)
            |> List.length
        List.init (List.length s) cardSelector

    let convertCardsToSiteswap cards =
        let p = List.length cards
        let card = (ModuloP p).Value >> List.nth cards
        let computeAi i =
            let rec helper j t =
                match card <| i+j with
                | c when c=t    -> j
                | c when c<t    -> helper (j+1) t
                | c             -> helper (j+1) (t+1)
            match card i with
            | 0 -> 0
            | _ -> helper 1 1
        List.init p computeAi