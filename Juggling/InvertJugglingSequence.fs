module InvertJugglingSequence

    open BasicJugglingFunctions

    let invert s =
        fun i -> List.findIndex ((=) i) <| phiOnSequenceInterval s
        |> List.init (List.length s)
        |> List.rev
        |> List.map (List.nth s)