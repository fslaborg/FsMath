namespace FsMath.Tests.LinearAlgebraTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle

open Microsoft.FSharp.Linq.RuntimeHelpers

// Quotation-based tests for inline LinearAlgebra functions
// These tests use F# quotation evaluation to ensure inline functions are tracked by coverage tools

module LinearAlgebraQuotationTests =

    // Helper to evaluate quotations
    let eval q = LeafExpressionConverter.EvaluateQuotation q

    // ============ qrModifiedGramSchmidt tests ============

    [<Fact>]
    let ``qrModifiedGramSchmidtQ: identity matrix`` () =
        let A = Matrix.identity<float> 3
        let (Q, R) = eval <@ LinearAlgebra.qrModifiedGramSchmidt<float> A @> :?> (Matrix<float> * Matrix<float>)
        let I = Matrix.identity<float> 3
        floatMatrixClose Accuracy.high Q I "Q should be identity"
        floatMatrixClose Accuracy.high R I "R should be identity"

    [<Fact>]
    let ``qrModifiedGramSchmidtQ: reconstruct A from Q * R`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                                1.0; 1.0;
                                0.0; 1.0 |])
        let (Q, R) = eval <@ LinearAlgebra.qrModifiedGramSchmidt A @> :?> (Matrix<float> * Matrix<float>)
        let QR = Matrix.matmul Q R
        floatMatrixClose Accuracy.high QR A "Q * R should reconstruct A"

    [<Fact>]
    let ``qrModifiedGramSchmidtQ: Q has orthonormal columns`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                                1.0; 1.0;
                                0.0; 1.0 |])
        let (Q, _) = eval <@ LinearAlgebra.qrModifiedGramSchmidt A @> :?> (Matrix<float> * Matrix<float>)
        let QTQ = Matrix.matmul (Matrix.transpose Q) Q
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.high QTQ I "Q^TQ should be identity"

    [<Fact>]
    let ``qrModifiedGramSchmidtQ: R is upper triangular`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                                1.0; 1.0;
                                0.0; 1.0 |])
        let (_, R) = eval <@ LinearAlgebra.qrModifiedGramSchmidt A @> :?> (Matrix<float> * Matrix<float>)
        for i in 0 .. R.NumRows - 1 do
            for j in 0 .. i - 1 do
                floatClose Accuracy.high R.[i, j] 0.0 $"R[{i},{j}] should be zero"

    [<Fact>]
    let ``qrModifiedGramSchmidtQ: throws on wide matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                                4.0; 5.0; 6.0 |])
        let action = eval <@ fun () -> LinearAlgebra.qrModifiedGramSchmidt A @> :?> (unit -> Matrix<float> * Matrix<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw on wide matrix"

    // ============ backSubstitute tests ============

    [<Fact>]
    let ``backSubstituteQ: simple upper triangular system`` () =
        let r = Matrix(2, 2, [| 2.0; 1.0;
                               0.0; 3.0 |])
        let y = [| 5.0; 6.0 |]
        let x = eval <@ LinearAlgebra.backSubstitute r y @> :?> Vector<float>
        let expected = [| 1.5; 2.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``backSubstituteQ: identity matrix`` () =
        let r = Matrix.identity 3
        let y = [| 4.0; -2.0; 0.5 |]
        let x = eval <@ LinearAlgebra.backSubstitute r y @> :?> Vector<float>
        Assert.Equal<Vector<float>>(y, x)

    [<Fact>]
    let ``backSubstituteQ: throws on non-square`` () =
        let r = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               0.0; 4.0; 5.0 |])
        let y = [| 1.0; 2.0 |]
        let action = eval <@ fun () -> LinearAlgebra.backSubstitute r y @> :?> (unit -> Vector<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ solveLinearQR tests ============

    [<Fact>]
    let ``solveLinearQRQ: square system`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let x = eval <@ LinearAlgebra.solveLinearQR A b @> :?> Vector<float>
        let expected = [| 1.0; 3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``solveLinearQRQ: throws on dimension mismatch`` () =
        let A = Matrix.identity 3
        let b = [| 1.0; 2.0 |]
        let action = eval <@ fun () -> LinearAlgebra.solveLinearQR A b @> :?> (unit -> Vector<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ solveTriangularLinearSystem tests ============

    [<Fact>]
    let ``solveTriangularLinearSystemQ: lower triangular`` () =
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0;
                               3.0; 4.0; 1.0 |])
        let b = [| 2.0; 5.0; 20.0 |]
        let x = eval <@ LinearAlgebra.solveTriangularLinearSystem L b true @> :?> Vector<float>
        let expected = [| 2.0; 1.0; 10.0 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``solveTriangularLinearSystemQ: upper triangular`` () =
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0;
                               3.0; 4.0; 1.0 |])
        let LT = Matrix.transpose L
        let b = [| 23.0; 9.0; 1.0 |]
        let x = eval <@ LinearAlgebra.solveTriangularLinearSystem LT b false @> :?> Vector<float>
        let expected = [| 10.0; 5.0; 1.0 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``solveTriangularLinearSystemQ: throws on non-square`` () =
        let L = Matrix(2, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0 |])
        let b = [| 1.0; 2.0 |]
        let action = eval <@ fun () -> LinearAlgebra.solveTriangularLinearSystem L b true @> :?> (unit -> Vector<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ solveTriangularLinearSystems tests ============

    [<Fact>]
    let ``solveTriangularLinearSystemsQ: lower triangular multiple RHS`` () =
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0;
                               3.0; 4.0; 1.0 |])
        let B = Matrix(3, 2, [| 2.0; 1.0;
                               5.0; 3.0;
                               20.0; 12.0 |])
        let X = eval <@ LinearAlgebra.solveTriangularLinearSystems L B true @> :?> Matrix<float>
        // First column solution
        floatEqual X.[0, 0] 2.0 1e-10
        floatEqual X.[1, 0] 1.0 1e-10
        floatEqual X.[2, 0] 10.0 1e-10

    [<Fact>]
    let ``solveTriangularLinearSystemsQ: upper triangular multiple RHS`` () =
        let U = Matrix(2, 2, [| 2.0; 1.0;
                               0.0; 3.0 |])
        let B = Matrix(2, 2, [| 5.0; 4.0;
                               6.0; 3.0 |])
        let X = eval <@ LinearAlgebra.solveTriangularLinearSystems U B false @> :?> Matrix<float>
        // Solutions should satisfy U * X = B
        let result = Matrix.matmul U X
        floatMatrixClose Accuracy.high result B "U * X should equal B"

    [<Fact>]
    let ``solveTriangularLinearSystemsQ: throws on non-square K`` () =
        let K = Matrix(2, 3, [| 1.0; 0.0; 0.0;
                               0.0; 1.0; 0.0 |])
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        let action = eval <@ fun () -> LinearAlgebra.solveTriangularLinearSystems K B true @> :?> (unit -> Matrix<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ qrDecompose (Householder) tests ============

    [<Fact>]
    let ``qrDecomposeQ: Q * R reconstructs A`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let (Q, R) = eval <@ LinearAlgebra.qrDecompose A @> :?> (Matrix<float> * Matrix<float>)
        let QR = Matrix.matmul Q R
        floatMatrixClose Accuracy.high QR A "Q * R should reconstruct A"

    [<Fact>]
    let ``qrDecomposeQ: Q is orthogonal`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                               1.0; 1.0;
                               1.0; 2.0 |])
        let (Q, _) = eval <@ LinearAlgebra.qrDecompose A @> :?> (Matrix<float> * Matrix<float>)
        let QtQ = Matrix.matmul (Matrix.transpose Q) Q
        let I = Matrix.identity Q.NumCols
        floatMatrixClose Accuracy.high QtQ I "Q^T Q should be identity"

    [<Fact>]
    let ``qrDecomposeQ: R is upper triangular`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                               2.0; 1.0;
                               3.0; 2.0 |])
        let (_, R) = eval <@ LinearAlgebra.qrDecompose A @> :?> (Matrix<float> * Matrix<float>)
        for i = 0 to R.NumRows - 1 do
            for j = 0 to i - 1 do
                floatEqual R.[i, j] 0.0 1e-12

    // ============ leastSquares tests ============

    [<Fact>]
    let ``leastSquaresQ: overdetermined 3x2 system`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let b = [| 6.0; 0.0; 0.0 |]
        let x = eval <@ LinearAlgebra.leastSquares A b @> :?> Vector<float>
        let expected = [| 8.0; -3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-6

    [<Fact>]
    let ``leastSquaresQ: underdetermined 2x3 system`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 0.0;
                               0.0; 1.0; 1.0 |])
        let b = [| 3.0; 2.0 |]
        let x = eval <@ LinearAlgebra.leastSquares A b @> :?> Vector<float>
        let expected = [| 0.33333333; 1.33333333; 0.66666667 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-6

    [<Fact>]
    let ``leastSquaresQ: square system`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let x = eval <@ LinearAlgebra.leastSquares A b @> :?> Vector<float>
        let expected = [| 1.0; 3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    // ============ cholesky tests ============

    [<Fact>]
    let ``choleskyQ: identity matrix`` () =
        let I = Matrix.identity 3
        let L = eval <@ LinearAlgebra.cholesky I @> :?> Matrix<float>
        floatMatrixClose Accuracy.high L I "L should be identity for identity input"

    [<Fact>]
    let ``choleskyQ: known positive-definite matrix`` () =
        let A = Matrix(2, 2, [| 4.0; 2.0;
                               2.0; 3.0 |])
        let L = eval <@ LinearAlgebra.cholesky A @> :?> Matrix<float>
        let expected = Matrix(2, 2, [| 2.0; 0.0;
                                      1.0; sqrt 2.0 |])
        for i in 0 .. 1 do
            for j in 0 .. 1 do
                floatEqual expected.[i,j] L.[i,j] 1e-5

    [<Fact>]
    let ``choleskyQ: reconstruct A from L * L^T`` () =
        let A = Matrix(3, 3, [| 25.0; 15.0; -5.0;
                               15.0; 18.0;  0.0;
                               -5.0; 0.0; 11.0 |])
        let L = eval <@ LinearAlgebra.cholesky A @> :?> Matrix<float>
        let LT = Matrix.transpose L
        let reconstructed = Matrix.matmul L LT
        floatMatrixClose Accuracy.high reconstructed A "Reconstructed A should match original"

    [<Fact>]
    let ``choleskyQ: throws on non-positive-definite`` () =
        let A = Matrix(2, 2, [| 0.0; 1.0;
                               1.0; 0.0 |])
        let action = eval <@ fun () -> LinearAlgebra.cholesky A @> :?> (unit -> Matrix<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ leastSquaresCholesky tests ============

    [<Fact>]
    let ``leastSquaresCholeskyQ: overdetermined 3x2 system`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let b = [| 6.0; 0.0; 0.0 |]
        let beta = eval <@ LinearAlgebra.leastSquaresCholesky A b @> :?> Vector<float>
        let expected = [| 8.0; -3.0 |]
        for i in 0..1 do
            floatEqual expected.[i] beta.[i] 1e-6

    [<Fact>]
    let ``leastSquaresCholeskyQ: exact solution when square and full-rank`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let beta = eval <@ LinearAlgebra.leastSquaresCholesky A b @> :?> Vector<float>
        let expected = [| 1.0; 3.0 |]
        for i in 0..1 do
            floatEqual expected.[i] beta.[i] 1e-10

    [<Fact>]
    let ``leastSquaresCholeskyQ: throws on dimension mismatch`` () =
        let A = Matrix.identity 3
        let b = [| 1.0; 2.0 |]
        let action = eval <@ fun () -> LinearAlgebra.leastSquaresCholesky A b @> :?> (unit -> Vector<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ hatMatrix tests ============

    [<Fact>]
    let ``hatMatrixQ: computes hat matrix for design matrix`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let H = eval <@ LinearAlgebra.hatMatrix X @> :?> Matrix<float>
        // Hat matrix should be square with size = number of rows in X
        Assert.Equal(4, H.NumRows)
        Assert.Equal(4, H.NumCols)
        // H should be idempotent: H * H = H
        let HH = Matrix.matmul H H
        floatMatrixClose Accuracy.medium HH H "H * H should equal H (idempotent)"

    [<Fact>]
    let ``hatMatrixQ: symmetric property`` () =
        let X = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let H = eval <@ LinearAlgebra.hatMatrix X @> :?> Matrix<float>
        let HT = Matrix.transpose H
        floatMatrixClose Accuracy.high H HT "Hat matrix should be symmetric"

    // ============ leverageBy tests ============

    [<Fact>]
    let ``leverageByQ: extracts diagonal from hat matrix`` () =
        let H = Matrix(3, 3, [| 0.5; 0.3; 0.2;
                               0.3; 0.4; 0.3;
                               0.2; 0.3; 0.5 |])
        let lev = eval <@ LinearAlgebra.leverageBy H @> :?> Vector<float>
        let expected = [| 0.5; 0.4; 0.5 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] lev.[i] 1e-10

    // ============ leverage tests ============

    [<Fact>]
    let ``leverageQ: computes leverage directly from design matrix`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let lev = eval <@ LinearAlgebra.leverage X @> :?> Vector<float>
        // Leverage values should be between 0 and 1
        Assert.Equal(4, lev.Length)
        for i in 0 .. 3 do
            Assert.True(lev.[i] >= 0.0 && lev.[i] <= 1.0, $"Leverage value {lev.[i]} should be in [0,1]")
        // Sum of leverages should equal the number of parameters
        let sum = Array.sum lev
        floatEqual sum 2.0 1e-10

    // ============ luDecompose tests ============

    [<Fact>]
    let ``luDecomposeQ: P * A = L * U`` () =
        let A = Matrix(3, 3, [| 2.0; 1.0; 1.0;
                               4.0; 3.0; 3.0;
                               8.0; 7.0; 9.0 |])
        let (P, L, U) = eval <@ LinearAlgebra.luDecompose A @> :?> (Permutation * Matrix<float> * Matrix<float>)
        let PA = A |> Matrix.permuteRowsBy P
        let LU = Matrix.matmul L U
        floatMatrixClose Accuracy.high PA LU "P * A should equal L * U"

    [<Fact>]
    let ``luDecomposeQ: L is lower triangular with unit diagonal`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let (_, L, _) = eval <@ LinearAlgebra.luDecompose A @> :?> (Permutation * Matrix<float> * Matrix<float>)
        // Check diagonal is 1.0
        for i in 0 .. L.NumRows - 1 do
            floatEqual L.[i, i] 1.0 1e-10
        // Check upper triangle is zero
        for i in 0 .. L.NumRows - 1 do
            for j in i + 1 .. L.NumCols - 1 do
                floatEqual L.[i, j] 0.0 1e-10

    [<Fact>]
    let ``luDecomposeQ: U is upper triangular`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let (_, _, U) = eval <@ LinearAlgebra.luDecompose A @> :?> (Permutation * Matrix<float> * Matrix<float>)
        // Check lower triangle is zero
        for i in 0 .. U.NumRows - 1 do
            for j in 0 .. i - 1 do
                floatEqual U.[i, j] 0.0 1e-10

    [<Fact>]
    let ``luDecomposeQ: throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let action = eval <@ fun () -> LinearAlgebra.luDecompose A @> :?> (unit -> Permutation * Matrix<float> * Matrix<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ solveLinearSystems tests ============

    [<Fact>]
    let ``solveLinearSystemsQ: solve A * X = B`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let B = Matrix(2, 2, [| 5.0; 7.0;
                               10.0; 13.0 |])
        let X = eval <@ LinearAlgebra.solveLinearSystems A B @> :?> Matrix<float>
        let result = Matrix.matmul A X
        floatMatrixClose Accuracy.high result B "A * X should equal B"

    [<Fact>]
    let ``solveLinearSystemsQ: throws on non-square A`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        let action = eval <@ fun () -> LinearAlgebra.solveLinearSystems A B @> :?> (unit -> Matrix<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ solveLinearSystem tests ============

    [<Fact>]
    let ``solveLinearSystemQ: solve A * x = b`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let x = eval <@ LinearAlgebra.solveLinearSystem A b @> :?> Vector<float>
        let expected = [| 1.0; 3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``solveLinearSystemQ: 3x3 system`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let b = [| 14.0; 32.0; 53.0 |]
        let x = eval <@ LinearAlgebra.solveLinearSystem A b @> :?> Vector<float>
        let result = Matrix.muliplyVector A x
        for i in 0 .. 2 do
            floatEqual b.[i] result.[i] 1e-9

    [<Fact>]
    let ``solveLinearSystemQ: throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let b = [| 1.0; 2.0 |]
        let action = eval <@ fun () -> LinearAlgebra.solveLinearSystem A b @> :?> (unit -> Vector<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ inverse tests ============

    [<Fact>]
    let ``inverseQ: identity matrix`` () =
        let I = Matrix.identity 3
        let Ainv = eval <@ LinearAlgebra.inverse I @> :?> Matrix<float>
        floatMatrixClose Accuracy.high Ainv I "Inverse of identity should be identity"

    [<Fact>]
    let ``inverseQ: A * A^-1 = I`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let Ainv = eval <@ LinearAlgebra.inverse A @> :?> Matrix<float>
        let result = Matrix.matmul A Ainv
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.high result I "A * A^-1 should be identity"

    [<Fact>]
    let ``inverseQ: 3x3 matrix`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let Ainv = eval <@ LinearAlgebra.inverse A @> :?> Matrix<float>
        let result = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium result I "A * A^-1 should be identity"

    [<Fact>]
    let ``inverseQ: throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let action = eval <@ fun () -> LinearAlgebra.inverse A @> :?> (unit -> Matrix<float>)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"

    // ============ pseudoInvers tests ============

    [<Fact>]
    let ``pseudoInversQ: overdetermined matrix`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let Apinv = eval <@ LinearAlgebra.pseudoInvers A @> :?> Matrix<float>
        // Pseudoinverse should have shape 2x3
        Assert.Equal(2, Apinv.NumRows)
        Assert.Equal(3, Apinv.NumCols)
        // A^+ * A should be close to identity (2x2)
        let ApinvA = Matrix.matmul Apinv A
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.low ApinvA I "A^+ * A should be close to identity"

    [<Fact>]
    let ``pseudoInversQ: underdetermined matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 0.0;
                               0.0; 1.0; 1.0 |])
        let Apinv = eval <@ LinearAlgebra.pseudoInvers A @> :?> Matrix<float>
        // Pseudoinverse should have shape 3x2
        Assert.Equal(3, Apinv.NumRows)
        Assert.Equal(2, Apinv.NumCols)
        // A * A^+ should be close to identity (2x2)
        let AApinv = Matrix.matmul A Apinv
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.low AApinv I "A * A^+ should be close to identity"

    [<Fact>]
    let ``pseudoInversQ: square matrix (should be regular inverse)`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let Apinv = eval <@ LinearAlgebra.pseudoInvers A @> :?> Matrix<float>
        let result = Matrix.matmul A Apinv
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.medium result I "For square matrix, A * A^+ = I"

    // ============ determinant tests ============

    [<Fact>]
    let ``determinantQ: identity matrix`` () =
        let I = Matrix.identity 3
        let det = eval <@ LinearAlgebra.determinant I @> :?> float
        floatEqual det 1.0 1e-10

    [<Fact>]
    let ``determinantQ: 2x2 matrix`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let det = eval <@ LinearAlgebra.determinant A @> :?> float
        // det = 2*3 - 1*1 = 5
        floatEqual det 5.0 1e-10

    [<Fact>]
    let ``determinantQ: 3x3 matrix`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let det = eval <@ LinearAlgebra.determinant A @> :?> float
        // det = -3
        floatEqual det -3.0 1e-10

    [<Fact>]
    let ``determinantQ: singular matrix has zero determinant`` () =
        let A = Matrix(2, 2, [| 1.0; 2.0;
                               2.0; 4.0 |])
        let det = eval <@ LinearAlgebra.determinant A @> :?> float
        floatEqual det 0.0 1e-10

    [<Fact>]
    let ``determinantQ: throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let action = eval <@ fun () -> LinearAlgebra.determinant A @> :?> (unit -> float)
        throws<ArgumentException> (fun () -> action() |> ignore) "Should throw ArgumentException"
