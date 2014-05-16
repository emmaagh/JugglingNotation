module PermuteQualifyingToJugglingTests
    
    open Xunit
    open BasicJugglingFunctions
    open PermuteQualifyingToJuggling

    let private testSequence s =
        let result = permuteQualifyingToJuggling s
        match s with
        | EmptySequence         -> Assert.Equal (None, result)
        | NonQualifyingSequence -> Assert.Equal (None, result)
        | _                     -> match result with
                                   | Some s'  -> Assert.True (isJuggleable s')
                                   | None     -> failwith "Failed to transform to juggling sequence"

    [<Fact>]
    let ``test odd sequences`` () =
        testSequence []

    [<Fact>]
    let ``test qualifying sequences`` () =
        testSequence [4]
        testSequence [2;10;3]
        testSequence [1;2;3;4;5]
        testSequence [1;2;3;3;1]
        testSequence [1;7;7;4;1]
        testSequence [5;5;5;4;6]
        testSequence [4;7;9;8;7;6;8]
        testSequence [6;8;6;8;7;6;8]
        testSequence [4;7;9;5;6;10;8]
        testSequence [4;6;10;7;8;5;9]
        testSequence [4;5;11;7;6;8;8]
        testSequence [4;7;9;18;2;1;8]
        testSequence [4;10;12;8;7;6;2]

    [<Fact>]
    let ``test non qualifying sequences`` () =
        testSequence [1;3;4;3]
        testSequence [2;1;5;4;7]
        testSequence [4;5;11;7;7;8;8]
        testSequence [4;7;6;18;2;1;8]