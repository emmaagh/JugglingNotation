module PermuteQualifyingToJuggling
    
    open ModularArithmetic
    open Siteswap
    open BasicJugglingFunctions

    type private PermuteToJuggling (inputSequence) =
        
        let p = List.length inputSequence
        let moduloP = ModuloP p
        let (=~) = moduloP.EqualsModP ()
        let (+~) = moduloP.AddModP ()
        let (-~) = moduloP.SubtractModP ()
    
        // Computes s^(n) which is equal to inputSequence for index < n, 0 for index > n and has value at
        // index = n chosen to make s^(n) equal to s in the case n = length inputSequence, and qualify otherwise
        let computeSn n =
            inputSequence
            |> List.mapi (fun i j -> if      i < n || n = p-1 then j
                                     else if i > n            then 0
                                     else                     inputSequence |> Seq.take n |> Seq.sum |> (~-) |> moduloP.Value)
    
        let rec permuteToJugglingSequence s j k d e =
            let getElementByIndex = List.nth s
            let aj = getElementByIndex j
            if d =~ j + aj || e =~ j + aj then
                id
            else if d =~ k + aj || e =~ k + aj then
                siteswap j k
            else
                let x = d -~ aj
                let siteswap' = siteswap j x
                siteswap' >> permuteToJugglingSequence (List.permute siteswap' s) j k e (x +~ getElementByIndex x)

        member m.Execute =
            let zeroSequence = List.replicate p 0
            let intermediateSequences = List.init (p-1) ((+) 1 >> computeSn)
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
        | EmptySequence         -> None
        | JugglingSequence      -> Some s
        | QualifyingSequence    -> Some <| (PermuteToJuggling s).Execute
        | NonQualifyingSequence -> None