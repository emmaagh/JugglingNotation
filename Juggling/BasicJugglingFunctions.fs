// Basic juggling related functions on sequences
module BasicJugglingFunctions

    open ModularArithmetic

    type private Sequence (inputSequence) =

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

        member m.CyclicPermutation n =
            inputSequence
            |> List.permute ((+) n >> moduloP.Value)

    let phi s = (Sequence s).Phi

    let qualifies s = (Sequence s).Qualifies

    let isJuggleable s = (Sequence s).IsJuggleable

    let (|EmptySequence|JugglingSequence|QualifyingSequence|NonQualifyingSequence|) s =
        match s with
        | []  -> EmptySequence
        | _   -> let s' = Sequence s
                 if      s'.IsJuggleable then JugglingSequence
                 else if s'.Qualifies    then QualifyingSequence
                 else                         NonQualifyingSequence

    let balls s =
        match s with
        | JugglingSequence  -> int <| List.averageBy float s
        | _                 -> failwith <| sprintf "%A is not a juggling sequence" s

    let equalUpToCyclicPermutation s1 s2 =
        Seq.init (List.length s1) id
        |> Seq.map (Sequence s1).CyclicPermutation
        |> Seq.exists ((=) s2)

    let getCyclicPermutations s =
        let s' = Sequence s
        s'.CyclicPermutation
        |> List.init (List.length s)