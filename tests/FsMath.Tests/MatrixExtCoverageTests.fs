namespace FsMath.Tests.Matrix

open System
open System.Linq.Expressions
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

/// <summary>
/// Quotation-based tests for inline extension methods in MatrixExt module.
/// These tests use F# quotation evaluation to force the coverage tool
/// to track inline function execution.
/// </summary>
module MatrixExtCoverageTests =

    /// Helper to evaluate F# quotations
    let inline eval<'T> (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        let linq = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression expr
        let lambda = Expression.Lambda<Func<'T>>(linq).Compile()
        lambda.Invoke()

    // ====== Inverse() quotation tests ======

    [<Fact>]
    let ``Inverse Q: 2x2 identity matrix returns identity`` () =
        let m = Matrix.identity<float> 2
        let inv = eval <@ m.Inverse() @>

        floatEqual 1.0 inv.[0, 0] 1e-10
        floatEqual 0.0 inv.[0, 1] 1e-10
        floatEqual 0.0 inv.[1, 0] 1e-10
        floatEqual 1.0 inv.[1, 1] 1e-10

    [<Fact>]
    let ``Inverse Q: 2x2 matrix with simple values`` () =
        // Matrix [[4, 7], [2, 6]] has inverse [[0.6, -0.7], [-0.2, 0.4]]
        let m = Matrix.ofArray2D (array2D [[4.0; 7.0]; [2.0; 6.0]])
        let inv = eval <@ m.Inverse() @>

        floatEqual 0.6 inv.[0, 0] 1e-10
        floatEqual -0.7 inv.[0, 1] 1e-10
        floatEqual -0.2 inv.[1, 0] 1e-10
        floatEqual 0.4 inv.[1, 1] 1e-10

    [<Fact>]
    let ``Inverse Q: A * A_inv equals identity`` () =
        let m = Matrix.ofArray2D (array2D [[2.0; 1.0]; [5.0; 3.0]])
        let inv = eval <@ m.Inverse() @>
        let product = m * inv

        // Check product is identity
        floatEqual 1.0 product.[0, 0] 1e-10
        floatEqual 0.0 product.[0, 1] 1e-10
        floatEqual 0.0 product.[1, 0] 1e-10
        floatEqual 1.0 product.[1, 1] 1e-10

    [<Fact>]
    let ``Inverse Q: A_inv * A equals identity`` () =
        let m = Matrix.ofArray2D (array2D [[3.0; 2.0]; [7.0; 5.0]])
        let inv = eval <@ m.Inverse() @>
        let product = inv * m

        // Check product is identity
        floatEqual 1.0 product.[0, 0] 1e-10
        floatEqual 0.0 product.[0, 1] 1e-10
        floatEqual 0.0 product.[1, 0] 1e-10
        floatEqual 1.0 product.[1, 1] 1e-10

    [<Fact>]
    let ``Inverse Q: 3x3 identity matrix`` () =
        let m = Matrix.identity<float> 3
        let inv = eval <@ m.Inverse() @>

        // Verify it's still identity
        for i in 0..2 do
            for j in 0..2 do
                if i = j then
                    floatEqual 1.0 inv.[i, j] 1e-10
                else
                    floatEqual 0.0 inv.[i, j] 1e-10

    [<Fact>]
    let ``Inverse Q: 3x3 matrix with known inverse`` () =
        // Matrix with known inverse
        let m = Matrix.ofArray2D (array2D [
            [1.0; 2.0; 3.0]
            [0.0; 1.0; 4.0]
            [5.0; 6.0; 0.0]
        ])
        let inv = eval <@ m.Inverse() @>

        // Verify A * A^-1 = I
        let product = m * inv
        for i in 0..2 do
            for j in 0..2 do
                if i = j then
                    floatEqual 1.0 product.[i, j] 1e-9
                else
                    floatEqual 0.0 product.[i, j] 1e-9

    [<Fact>]
    let ``Inverse Q: 4x4 identity matrix`` () =
        let m = Matrix.identity<float> 4
        let inv = eval <@ m.Inverse() @>

        Assert.Equal(4, inv.NumRows)
        Assert.Equal(4, inv.NumCols)

        for i in 0..3 do
            for j in 0..3 do
                if i = j then
                    floatEqual 1.0 inv.[i, j] 1e-10
                else
                    floatEqual 0.0 inv.[i, j] 1e-10

    [<Fact>]
    let ``Inverse Q: 4x4 matrix verification`` () =
        // Use a well-conditioned 4x4 matrix
        let m = Matrix.ofArray2D (array2D [
            [4.0; 0.0; 0.0; 1.0]
            [0.0; 3.0; 0.0; 0.0]
            [0.0; 0.0; 2.0; 0.0]
            [1.0; 0.0; 0.0; 1.0]
        ])
        let inv = eval <@ m.Inverse() @>

        // Verify A * A^-1 = I
        let product = m * inv
        for i in 0..3 do
            for j in 0..3 do
                if i = j then
                    floatEqual 1.0 product.[i, j] 1e-9
                else
                    floatEqual 0.0 product.[i, j] 1e-9

    [<Fact>]
    let ``Inverse Q: double inverse returns original`` () =
        let m = Matrix.ofArray2D (array2D [[4.0; 3.0]; [3.0; 2.0]])
        let inv = eval <@ m.Inverse() @>
        let invInv = eval <@ inv.Inverse() @>

        // (A^-1)^-1 should equal A
        floatEqual m.[0, 0] invInv.[0, 0] 1e-9
        floatEqual m.[0, 1] invInv.[0, 1] 1e-9
        floatEqual m.[1, 0] invInv.[1, 0] 1e-9
        floatEqual m.[1, 1] invInv.[1, 1] 1e-9

    [<Fact>]
    let ``Inverse Q: single element matrix`` () =
        let m = Matrix.ofArray2D (array2D [[5.0]])
        let inv = eval <@ m.Inverse() @>

        floatEqual 0.2 inv.[0, 0] 1e-10

    [<Fact>]
    let ``Inverse Q: diagonal matrix`` () =
        let m = Matrix.ofArray2D (array2D [
            [2.0; 0.0; 0.0]
            [0.0; 4.0; 0.0]
            [0.0; 0.0; 5.0]
        ])
        let inv = eval <@ m.Inverse() @>

        // Diagonal inverse should be 1/diagonal
        floatEqual 0.5 inv.[0, 0] 1e-10
        floatEqual 0.25 inv.[1, 1] 1e-10
        floatEqual 0.2 inv.[2, 2] 1e-10

        // Off-diagonal should be zero
        floatEqual 0.0 inv.[0, 1] 1e-10
        floatEqual 0.0 inv.[0, 2] 1e-10
        floatEqual 0.0 inv.[1, 0] 1e-10

    [<Fact>]
    let ``Inverse Q: throws on non-square matrix (more rows)`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]; [5.0; 6.0]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ m.Inverse() @> |> ignore)

    [<Fact>]
    let ``Inverse Q: throws on non-square matrix (more cols)`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ m.Inverse() @> |> ignore)

    [<Fact>]
    let ``Inverse Q: preserves matrix dimensions`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0; 3.0]; [0.0; 1.0; 4.0]; [5.0; 6.0; 0.0]])
        let inv = eval <@ m.Inverse() @>

        Assert.Equal(m.NumRows, inv.NumRows)
        Assert.Equal(m.NumCols, inv.NumCols)

    [<Fact>]
    let ``Inverse Q: 5x5 matrix`` () =
        // Create a well-conditioned 5x5 matrix
        let m = Matrix.ofArray2D (array2D [
            [5.0; 0.0; 0.0; 0.0; 1.0]
            [0.0; 4.0; 0.0; 0.0; 0.0]
            [0.0; 0.0; 3.0; 0.0; 0.0]
            [0.0; 0.0; 0.0; 2.0; 0.0]
            [1.0; 0.0; 0.0; 0.0; 1.0]
        ])
        let inv = eval <@ m.Inverse() @>

        // Verify it's truly an inverse
        let product = m * inv
        for i in 0..4 do
            for j in 0..4 do
                if i = j then
                    floatEqual 1.0 product.[i, j] 1e-9
                else
                    floatEqual 0.0 product.[i, j] 1e-9

    [<Fact>]
    let ``Inverse Q: handles negative values`` () =
        let m = Matrix.ofArray2D (array2D [[-2.0; 1.0]; [1.0; -1.0]])
        let inv = eval <@ m.Inverse() @>

        let product = m * inv
        floatEqual 1.0 product.[0, 0] 1e-10
        floatEqual 0.0 product.[0, 1] 1e-10
        floatEqual 0.0 product.[1, 0] 1e-10
        floatEqual 1.0 product.[1, 1] 1e-10

    [<Fact>]
    let ``Inverse Q: handles mixed positive and negative values`` () =
        let m = Matrix.ofArray2D (array2D [
            [3.0; -1.0; 2.0]
            [-1.0; 2.0; -1.0]
            [2.0; -1.0; 3.0]
        ])
        let inv = eval <@ m.Inverse() @>

        let product = m * inv
        for i in 0..2 do
            for j in 0..2 do
                if i = j then
                    floatEqual 1.0 product.[i, j] 1e-9
                else
                    floatEqual 0.0 product.[i, j] 1e-9

    [<Fact>]
    let ``Inverse Q: result has correct type`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let inv = eval <@ m.Inverse() @>

        Assert.IsType<Matrix<float>>(inv) |> ignore

    [<Fact>]
    let ``Inverse Q: works with different numeric types - int`` () =
        // Note: integer matrix inverse may not be exact due to rounding
        let m = Matrix.ofArray2D (array2D [[4; 7]; [2; 6]])
        let inv = eval <@ m.Inverse() @>

        // For integer matrices, the inverse computation uses the generic number operations
        // Just verify it doesn't crash and returns a matrix of the right size
        Assert.Equal(2, inv.NumRows)
        Assert.Equal(2, inv.NumCols)
