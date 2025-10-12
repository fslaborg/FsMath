namespace FsMath.Tests

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers


module AlgTypesTopLevelOpsTests =

    [<Fact>]
    let ``matrix: creates matrix from list of lists`` () =
        let m = matrix [[1.0; 2.0]; [3.0; 4.0]]
        Assert.Equal(2, m.NumRows)
        Assert.Equal(2, m.NumCols)
        floatEqual 1.0 m.[0, 0] 1e-10
        floatEqual 2.0 m.[0, 1] 1e-10
        floatEqual 3.0 m.[1, 0] 1e-10
        floatEqual 4.0 m.[1, 1] 1e-10

    [<Fact>]
    let ``matrix: creates single-element matrix`` () =
        let m = matrix [[5.0]]
        Assert.Equal(1, m.NumRows)
        Assert.Equal(1, m.NumCols)
        floatEqual 5.0 m.[0, 0] 1e-10

    [<Fact>]
    let ``matrix: creates from single row`` () =
        let m = matrix [[1.0; 2.0; 3.0]]
        Assert.Equal(1, m.NumRows)
        Assert.Equal(3, m.NumCols)
        floatEqual 1.0 m.[0, 0] 1e-10
        floatEqual 2.0 m.[0, 1] 1e-10
        floatEqual 3.0 m.[0, 2] 1e-10

    [<Fact>]
    let ``matrix: creates from single column`` () =
        let m = matrix [[1.0]; [2.0]; [3.0]]
        Assert.Equal(3, m.NumRows)
        Assert.Equal(1, m.NumCols)
        floatEqual 1.0 m.[0, 0] 1e-10
        floatEqual 2.0 m.[1, 0] 1e-10
        floatEqual 3.0 m.[2, 0] 1e-10

    [<Fact>]
    let ``matrix: creates larger matrix`` () =
        let m = matrix [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]; [7.0; 8.0; 9.0]]
        Assert.Equal(3, m.NumRows)
        Assert.Equal(3, m.NumCols)
        floatEqual 5.0 m.[1, 1] 1e-10
        floatEqual 9.0 m.[2, 2] 1e-10

    [<Fact>]
    let ``matrix: with negative values`` () =
        let m = matrix [[-1.0; -2.0]; [-3.0; -4.0]]
        floatEqual -1.0 m.[0, 0] 1e-10
        floatEqual -4.0 m.[1, 1] 1e-10

    [<Fact>]
    let ``matrix: with zero values`` () =
        let m = matrix [[0.0; 0.0]; [0.0; 0.0]]
        floatEqual 0.0 m.[0, 0] 1e-10
        floatEqual 0.0 m.[1, 1] 1e-10

    [<Fact>]
    let ``matrix: with mixed values`` () =
        let m = matrix [[1.5; -2.3; 0.0]; [3.7; 0.0; -1.1]]
        floatEqual 1.5 m.[0, 0] 1e-10
        floatEqual -2.3 m.[0, 1] 1e-10
        floatEqual 0.0 m.[0, 2] 1e-10
        floatEqual 3.7 m.[1, 0] 1e-10
        floatEqual -1.1 m.[1, 2] 1e-10

    [<Fact>]
    let ``vector: creates vector from list`` () =
        let v = vector [1.0; 2.0; 3.0]
        Assert.Equal(3, v.Length)
        floatEqual 1.0 v.[0] 1e-10
        floatEqual 2.0 v.[1] 1e-10
        floatEqual 3.0 v.[2] 1e-10

    [<Fact>]
    let ``vector: creates single-element vector`` () =
        let v = vector [5.0]
        Assert.Equal(1, v.Length)
        floatEqual 5.0 v.[0] 1e-10

    [<Fact>]
    let ``vector: creates larger vector`` () =
        let v = vector [1.0; 2.0; 3.0; 4.0; 5.0; 6.0]
        Assert.Equal(6, v.Length)
        floatEqual 1.0 v.[0] 1e-10
        floatEqual 6.0 v.[5] 1e-10

    [<Fact>]
    let ``vector: with negative values`` () =
        let v = vector [-1.0; -2.0; -3.0]
        floatEqual -1.0 v.[0] 1e-10
        floatEqual -2.0 v.[1] 1e-10
        floatEqual -3.0 v.[2] 1e-10

    [<Fact>]
    let ``vector: with zero values`` () =
        let v = vector [0.0; 0.0; 0.0]
        floatEqual 0.0 v.[0] 1e-10
        floatEqual 0.0 v.[1] 1e-10
        floatEqual 0.0 v.[2] 1e-10

    [<Fact>]
    let ``vector: with mixed values`` () =
        let v = vector [1.5; -2.3; 0.0; 3.7]
        floatEqual 1.5 v.[0] 1e-10
        floatEqual -2.3 v.[1] 1e-10
        floatEqual 0.0 v.[2] 1e-10
        floatEqual 3.7 v.[3] 1e-10

    [<Fact>]
    let ``matrix and vector: interoperability`` () =
        // Create a matrix and use it with vector operations
        let m = matrix [[1.0; 2.0]; [3.0; 4.0]]
        let v = vector [1.0; 2.0]

        // Basic verification that types are compatible
        Assert.Equal(m.NumCols, v.Length)

    [<Fact>]
    let ``matrix: preserves floating point precision`` () =
        let precise = 1.23456789012345
        let m = matrix [[precise]]
        floatEqual precise m.[0, 0] 1e-10

    [<Fact>]
    let ``vector: preserves floating point precision`` () =
        let precise = 1.23456789012345
        let v = vector [precise]
        floatEqual precise v.[0] 1e-10

    [<Fact>]
    let ``matrix: creates from sequence expressions`` () =
        let rows = [ for i in 1..2 -> [ for j in 1..3 -> float (i * 10 + j) ] ]
        let m = matrix rows
        Assert.Equal(2, m.NumRows)
        Assert.Equal(3, m.NumCols)
        floatEqual 11.0 m.[0, 0] 1e-10
        floatEqual 23.0 m.[1, 2] 1e-10

    [<Fact>]
    let ``vector: creates from sequence expression`` () =
        let v = vector [ for i in 1..5 -> float i ]
        Assert.Equal(5, v.Length)
        floatEqual 1.0 v.[0] 1e-10
        floatEqual 5.0 v.[4] 1e-10
