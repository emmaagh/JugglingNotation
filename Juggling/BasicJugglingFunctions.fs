// Basic juggling related functions on sequences
module BasicJugglingFunctions

    open ModularArithmetic

    type private Sequence (inputSequence) =

        let p = List.length inputSequence
        let moduloP = ModuloP p
        let (+~) = moduloP.AddModP ()

        let isNonNegative = List.forall ((<=) 0) inputSequence
        let hasIntegerAverage = 0 = (moduloP.Value <| List.sum inputSequence)

        let jugglingFunction i = inputSequence.[moduloP.Value i]

        member m.Height = List.max inputSequence

        member m.PhiOnSequenceInterval = List.mapi (+~) inputSequence

        member m.Phi i = i + jugglingFunction i

        member m.PhiIsJugglingFn =
            m.PhiOnSequenceInterval
            |> List.sort
            |> List.forall2 (=) (List.init p id)

        member m.Qualifies = isNonNegative && hasIntegerAverage

        member m.IsJuggleable = isNonNegative && m.PhiIsJugglingFn

        member m.CyclicPermutation n =
            inputSequence
            |> List.permute ((+) n >> moduloP.Value)

        member m.BallLandingsWithinInterval start ``end`` =
            let length = ``end`` - start + 1
            // TODO: Make algorithm work without this restriction
            if length < m.Height then failwith "Interval too small to work over - length must be at least the height of the sequence (as algorithm needs to guarantee ever ball lands in the interval)"
            let getLandings =
                let rec helper landings i =
                    if i > ``end`` then
                        landings
                    else
                        helper <| Set.add i landings <| m.Phi i
                helper Set.empty
            let interval = List.init length ((+) start)
            let rec getClasses classes =
                match interval |> List.filter (fun i -> classes |> Set.unionMany |> Set.contains i |> not && jugglingFunction i <> 0) with
                | []    -> classes
                | i::_  -> getLandings i :: classes |> getClasses
            getClasses []

        member m.IndexOfSender i =
            let rec helper j =
                if m.Phi j = i then j
                else helper <| j-1
            helper i

    let height s = (Sequence s).Height

    let phiOnSequenceInterval s = (Sequence s).PhiOnSequenceInterval
    
    let phi s = (Sequence s).Phi

    let qualifies s = (Sequence s).Qualifies

    let isJuggleable s = (Sequence s).IsJuggleable

    let (|EmptySequence|JugglingSequence|QualifyingSequence|NonQualifyingSequence|) = function
        | []  -> EmptySequence
        | s   -> let s' = Sequence s
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
        (Sequence s).CyclicPermutation
        |> List.init (List.length s)
        |> Set.ofList

    // The classes of ball landings within the interval (inclusive of endpoints)
    let getBallLandingsWithinInterval s = (Sequence s).BallLandingsWithinInterval

    let indexOfSender s = (Sequence s).IndexOfSender