module PermutationGenerationTests

    open Xunit
    open GeneratePermutations

    [<Fact>]
    let ``Test 1`` () =
        let permutations = generatePermutations 1
        Assert.Equal (1, Set.count permutations)
        Set.iter (fun p -> Assert.Equal<int list> ([0], p)) permutations

    [<Fact>]
    let ``Test 2`` () =
        let permutations = generatePermutations 2
        Assert.Equal<Set<int list>> (Set.ofList [[0;1]; [1;0]], permutations)

    [<Fact>]
    let ``Test 3`` () =
        let permutations = generatePermutations 3
        Assert.Equal<Set<int list>> (Set.ofList [[0;1;2]; [0;2;1]; [1;0;2]; [1;2;0]; [2;0;1]; [2;1;0]], permutations)