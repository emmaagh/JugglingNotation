// Basic juggling related functions on sequences
module BasicJugglingFunctions

    open ModularArithmetic

    type private SequenceWithBasicJugglingDefinitionCapabilities (inputSequence) =

        let p = List.length inputSequence
        let moduloP = ModuloP p
        let (+~) = moduloP.AddModP ()

        let isNonNegative = List.forall ((<=) 0) inputSequence
        let hasIntegerAverage = 0 = (moduloP.Value <| List.sum inputSequence)

        member m.Phi = List.mapi (+~) inputSequence

        member m.PhiIsJugglingFn =
            m.Phi
            |> List.sort
            |> List.forall2 (=) (List.init p id)

        member m.Qualifies = isNonNegative && hasIntegerAverage

        member m.IsJuggleable = isNonNegative && m.PhiIsJugglingFn

    let phi s = (SequenceWithBasicJugglingDefinitionCapabilities s).Phi

    let qualifies s = (SequenceWithBasicJugglingDefinitionCapabilities s).Qualifies

    let isJuggleable s = (SequenceWithBasicJugglingDefinitionCapabilities s).IsJuggleable

    let (|EmptySequence|JugglingSequence|QualifyingSequence|NonQualifyingSequence|) s =
        match s with
        | []  -> EmptySequence
        | _   -> let s' = SequenceWithBasicJugglingDefinitionCapabilities s
                 if      s'.IsJuggleable then JugglingSequence
                 else if s'.Qualifies    then QualifyingSequence
                 else                         NonQualifyingSequence

    let balls s =
        match s with
        | JugglingSequence  -> (int) <| List.averageBy float s
        | _                 -> failwith <| sprintf "%A is not a juggling sequence" s