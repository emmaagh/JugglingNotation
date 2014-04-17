module ModularArithmetic

    // Computes n mod p, returning a non negative value < p
    let modulo p n = ((n % p) + p) % p

    type ModuloP (p) =
        let moduloP = modulo p
        member m.EqualsModP ()   = fun a b -> 0 = (modulo p <| a - b)
        member m.AddModP ()      = fun a b -> moduloP <| a + b
        member m.SubtractModP () = fun a b -> moduloP <| a - b
        member m.Value (a)       = moduloP a