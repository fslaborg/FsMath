namespace FsMath.Tests.AlgTypesTopLevelOps

open System
open Xunit
open FsMath
open Microsoft.FSharp.Linq.RuntimeHelpers

/// <summary>
/// Quotation-based tests for AlgTypesTopLevelOps inline helper functions.
///
/// These tests use F# quotation evaluation to ensure that inline functions
/// are properly tracked by coverage tools. This technique was recommended
/// by maintainers in discussion #5.
/// </summary>
module AlgTypesTopLevelOpsQuotationTests =

    /// Helper to evaluate quotations for coverage tracking
    let inline eval<'T> q = LeafExpressionConverter.EvaluateQuotation q :?> 'T

    // ========================================
    // matrix function tests
    // ========================================

    [<Fact>]
    let ``matrix: creates 2x2 matrix from list of lists`` () =
        let m = eval<Matrix<float>> <@ matrix [[1.0; 2.0]; [3.0; 4.0]] @>
        Assert.Equal(2, m.NumRows)
        Assert.Equal(2, m.NumCols)
        Assert.Equal(1.0, m.[0, 0])
        Assert.Equal(2.0, m.[0, 1])
        Assert.Equal(3.0, m.[1, 0])
        Assert.Equal(4.0, m.[1, 1])

    [<Fact>]
    let ``matrix: creates 3x3 identity-like matrix`` () =
        let m = eval<Matrix<float>> <@ matrix [[1.0; 0.0; 0.0]; [0.0; 1.0; 0.0]; [0.0; 0.0; 1.0]] @>
        Assert.Equal(3, m.NumRows)
        Assert.Equal(3, m.NumCols)
        // Check diagonal
        Assert.Equal(1.0, m.[0, 0])
        Assert.Equal(1.0, m.[1, 1])
        Assert.Equal(1.0, m.[2, 2])
        // Check off-diagonal zeros
        Assert.Equal(0.0, m.[0, 1])
        Assert.Equal(0.0, m.[1, 0])

    [<Fact>]
    let ``matrix: creates 1x1 matrix with single element`` () =
        let m = eval<Matrix<float>> <@ matrix [[42.0]] @>
        Assert.Equal(1, m.NumRows)
        Assert.Equal(1, m.NumCols)
        Assert.Equal(42.0, m.[0, 0])

    [<Fact>]
    let ``matrix: creates 1x3 row matrix`` () =
        let m = eval<Matrix<int>> <@ matrix [[1; 2; 3]] @>
        Assert.Equal(1, m.NumRows)
        Assert.Equal(3, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(2, m.[0, 1])
        Assert.Equal(3, m.[0, 2])

    [<Fact>]
    let ``matrix: creates 3x1 column matrix`` () =
        let m = eval<Matrix<int>> <@ matrix [[1]; [2]; [3]] @>
        Assert.Equal(3, m.NumRows)
        Assert.Equal(1, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(2, m.[1, 0])
        Assert.Equal(3, m.[2, 0])

    [<Fact>]
    let ``matrix: creates matrix with negative values`` () =
        let m = eval<Matrix<float>> <@ matrix [[-1.0; -2.0]; [-3.0; -4.0]] @>
        Assert.Equal(-1.0, m.[0, 0])
        Assert.Equal(-2.0, m.[0, 1])
        Assert.Equal(-3.0, m.[1, 0])
        Assert.Equal(-4.0, m.[1, 1])

    [<Fact>]
    let ``matrix: creates matrix with mixed positive and negative`` () =
        let m = eval<Matrix<float>> <@ matrix [[1.0; -2.0]; [-3.0; 4.0]] @>
        Assert.Equal(1.0, m.[0, 0])
        Assert.Equal(-2.0, m.[0, 1])
        Assert.Equal(-3.0, m.[1, 0])
        Assert.Equal(4.0, m.[1, 1])

    [<Fact>]
    let ``matrix: works with integer type`` () =
        let m = eval<Matrix<int>> <@ matrix [[1; 2]; [3; 4]] @>
        Assert.Equal(2, m.NumRows)
        Assert.Equal(2, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(4, m.[1, 1])

    [<Fact>]
    let ``matrix: creates zero matrix`` () =
        let m = eval<Matrix<float>> <@ matrix [[0.0; 0.0]; [0.0; 0.0]] @>
        Assert.Equal(2, m.NumRows)
        Assert.Equal(2, m.NumCols)
        Assert.Equal(0.0, m.[0, 0])
        Assert.Equal(0.0, m.[0, 1])
        Assert.Equal(0.0, m.[1, 0])
        Assert.Equal(0.0, m.[1, 1])

    [<Fact>]
    let ``matrix: creates large matrix 5x5`` () =
        let m = eval<Matrix<float>> <@ matrix [
            [1.0; 2.0; 3.0; 4.0; 5.0]
            [6.0; 7.0; 8.0; 9.0; 10.0]
            [11.0; 12.0; 13.0; 14.0; 15.0]
            [16.0; 17.0; 18.0; 19.0; 20.0]
            [21.0; 22.0; 23.0; 24.0; 25.0]
        ] @>
        Assert.Equal(5, m.NumRows)
        Assert.Equal(5, m.NumCols)
        Assert.Equal(1.0, m.[0, 0])
        Assert.Equal(25.0, m.[4, 4])
        Assert.Equal(13.0, m.[2, 2])

    // ========================================
    // vector function tests
    // ========================================

    [<Fact>]
    let ``vector: creates vector from 3-element list`` () =
        let v = eval<Vector<float>> <@ vector [1.0; 2.0; 3.0] @>
        Assert.Equal(3, v.Length)
        Assert.Equal(1.0, v.[0])
        Assert.Equal(2.0, v.[1])
        Assert.Equal(3.0, v.[2])

    [<Fact>]
    let ``vector: creates vector from single element`` () =
        let v = eval<Vector<float>> <@ vector [42.0] @>
        Assert.Equal(1, v.Length)
        Assert.Equal(42.0, v.[0])

    [<Fact>]
    let ``vector: creates vector with negative values`` () =
        let v = eval<Vector<float>> <@ vector [-1.0; -2.0; -3.0] @>
        Assert.Equal(3, v.Length)
        Assert.Equal(-1.0, v.[0])
        Assert.Equal(-2.0, v.[1])
        Assert.Equal(-3.0, v.[2])

    [<Fact>]
    let ``vector: creates vector with mixed positive and negative`` () =
        let v = eval<Vector<float>> <@ vector [1.0; -2.0; 3.0; -4.0] @>
        Assert.Equal(4, v.Length)
        Assert.Equal(1.0, v.[0])
        Assert.Equal(-2.0, v.[1])
        Assert.Equal(3.0, v.[2])
        Assert.Equal(-4.0, v.[3])

    [<Fact>]
    let ``vector: works with integer type`` () =
        let v = eval<Vector<int>> <@ vector [1; 2; 3; 4; 5] @>
        Assert.Equal(5, v.Length)
        Assert.Equal(1, v.[0])
        Assert.Equal(5, v.[4])

    [<Fact>]
    let ``vector: creates zero vector`` () =
        let v = eval<Vector<float>> <@ vector [0.0; 0.0; 0.0] @>
        Assert.Equal(3, v.Length)
        Assert.Equal(0.0, v.[0])
        Assert.Equal(0.0, v.[1])
        Assert.Equal(0.0, v.[2])

    [<Fact>]
    let ``vector: creates large vector`` () =
        let v = eval<Vector<float>> <@ vector [1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0] @>
        Assert.Equal(10, v.Length)
        Assert.Equal(1.0, v.[0])
        Assert.Equal(10.0, v.[9])
        Assert.Equal(5.0, v.[4])

    [<Fact>]
    let ``vector: creates vector with decimal values`` () =
        let v = eval<Vector<float>> <@ vector [1.5; 2.7; 3.9] @>
        Assert.Equal(3, v.Length)
        Assert.Equal(1.5, v.[0])
        Assert.Equal(2.7, v.[1])
        Assert.Equal(3.9, v.[2])

    [<Fact>]
    let ``vector: result type is Vector`` () =
        let v = eval<Vector<float>> <@ vector [1.0; 2.0] @>
        Assert.IsType<Vector<float>>(v) |> ignore

    [<Fact>]
    let ``matrix: result type is Matrix`` () =
        let m = eval<Matrix<float>> <@ matrix [[1.0; 2.0]] @>
        Assert.IsType<Matrix<float>>(m) |> ignore
