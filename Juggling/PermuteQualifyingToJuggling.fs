module PermuteQualifyingToJuggling
    
    open ModularArithmetic
    open Siteswap
    open BasicJugglingFunctions

    type TransformationToJugglingSequenceResult =
        | Success of int list
        | Failure of string

    type private QualifyingSequenceWithPermuteToJugglingCapability (inputSequence) =
        
        let p = List.length inputSequence
        let moduloP = ModuloP p
        let (=~) = moduloP.EqualsModP ()
        let (+~) = moduloP.AddModP ()
        let (-~) = moduloP.SubtractModP ()
    
        // Computes s^(n) which is equal to s for index < n, 0 for index > n and has value at
        // index = n chosen to make s^(n) equal to s in the case n = length s, and qualify otherwise
        let sn s n =
            s
            |> List.mapi (fun i j -> if      i < n || n = p-1 then j
                                     else if i > n            then 0
                                     else                     let x = s |> Seq.take n |> Seq.sum
                                                              moduloP.Value <| -x)
    
        let rec permuteToJugglingSequence s j k d e =
            let element = List.nth s
            let aj = element j
            if d =~ j + aj || e =~ j + aj then
                id
            else if d =~ k + aj || e =~ k + aj then
                siteswap j k
            else
                let x = d -~ aj
                let siteswap' = siteswap j x
                siteswap' >> permuteToJugglingSequence (List.permute siteswap' s) j k e (x +~ element x)

        member m.PermuteQualifyingToJuggling =
            let zeroSequence = List.replicate p 0
            let intermediateSequences = List.init (p-1) ((+) 1 >> sn inputSequence)
            let _, _, permutation =
                List.fold
                    (fun (previous, n, permute) current -> let j = permute n
                                                           let k = permute <| n + 1
                                                           let previous' = List.permute permute previous
                                                           let current'  = List.permute permute current
                                                           let d = j +~ List.nth previous' j
                                                           let e = k +~ List.nth previous' k
                                                           current, n+1, permute >> permuteToJugglingSequence current' j k d e)
                    (zeroSequence, 0, id)
                    intermediateSequences
            List.permute permutation inputSequence

    // Permutes an input qualifying sequence to a juggling sequence
    let permuteQualifyingToJuggling s =
        match s with
        | EmptySequence         -> Failure "Cannot juggle nothing"
        | JugglingSequence      -> Success s
        | QualifyingSequence    -> Success <| (QualifyingSequenceWithPermuteToJugglingCapability s).PermuteQualifyingToJuggling
        | NonQualifyingSequence -> Failure "Input sequence does not qualify"