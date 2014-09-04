module ModularArithmetic

    open System

    // Computes n mod p, returning a non negative value < p
    let modulo p n = ((n % p) + p) % p

    [<CustomEquality>]
    [<CustomComparison>]
    type ModuloP =
        {
            period : int;
            v      : int
        }

        override m.Equals o =
            match o with
            | :? ModuloP as m' -> let moduloP = modulo m.period
                                  m.period = m'.period
                                  && moduloP m.value = moduloP m'.value
            | _                 -> false
        
        override m.GetHashCode () = modulo m.period m.value

        interface IComparable with
            member m.CompareTo o =
                match o with
                | :? ModuloP as m' -> m.period.CompareTo m'.period
                | _                -> invalidArg "o" "Cannot compare value of different type"

        static member (+) (m, m') =
            if m.period = m'.period then
                {   period = m.period;
                    v      = modulo m.period <| m.value + m'.value  }
            else
                failwith "p and p' not equal"
        
        static member (-) (m, m') =
            if m.period = m'.period then
                {   period = m.period;
                    v      = modulo m.period <| m.value - m'.value  }
            else
                failwith "p and p' not equal"

        static member (+.) (i, m) =
            {   period = m.period;
                v      = modulo m.period <| i + m.value }

        static member (-.) (m, i) =
            {   period = m.period;
                v      = modulo m.period <| m.value - i }

        member m.value = modulo m.period m.v