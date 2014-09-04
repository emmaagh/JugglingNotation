module PermuteQualifyingToJuggling
    
    open ModularArithmetic
    open Siteswap
    open BasicJugglingFunctions

    // Computes s^(n) which is equal to inputSequence for index < n, 0 for index > n and has value at
    // index = n chosen to make s^(n) equal to s in the case n = length inputSequence, and qualify otherwise
    let private computeSn s n =
        let p = period s
        s
        |> mapToValuesModP
        |> List.mapi (fun i j -> if      i < n || n = p-1 then j
                                 else if i > n            then { period = p; v = 0 }
                                 else                     s |> Seq.take n |> Seq.sum |> (~-) |> fun v -> { period = p; v = v })

    let rec private permuteToJugglingSequence s j k d (e:ModuloP) =
        let getElementByIndex = List.nth s
        let aj = getElementByIndex j
        if d = j +. aj || e = j +. aj then
            id
        else if d = k +. aj || e = k +. aj then
            siteswap j k
        else
            let x = d - aj
            let siteswap' = siteswap j x.value
            siteswap' >> permuteToJugglingSequence (List.permute siteswap' s) j k e (getElementByIndex x.value + x)

    let private permuteToJuggling s =
        let p = period s
        let zeroSequence = List.replicate p { period = p; v = 0 }
        let intermediateSequences = List.init (p-1) ((+) 1 >> computeSn s)
        let _, _, permutation =
            List.fold
                (fun ((previous:ModuloP list), n, permute) current ->
                                                        let j = permute n
                                                        let k = permute <| n + 1
                                                        let previous' = List.permute permute previous
                                                        let current'  = List.permute permute current
                                                        let d = j +. List.nth previous' j
                                                        let e = k +. List.nth previous' k
                                                        current, n+1, permute >> permuteToJugglingSequence current' j k d e)
                (zeroSequence, 0, id)
                intermediateSequences
        List.permute permutation s

    // Permutes an input qualifying sequence to a juggling sequence
    let permuteQualifyingToJuggling s =
        match s with
        | EmptySequence         -> None
        | JugglingSequence      -> Some s
        | QualifyingSequence    -> Some <| permuteToJuggling s
        | NonQualifyingSequence -> None