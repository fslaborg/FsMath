namespace FsMath.Tests.LinearAlgebraTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle


module SubScaledRowInPlaceTests =

    [<Fact>]
    let ``Basic subtraction (no offset)`` () =
        let dst = [| 10.0; 20.0; 30.0 |]
        let src = [| 1.0; 2.0; 3.0 |]
        LinearAlgebra.subScaledRowInPlace 2.0 0 0 3 dst src
        Assert.Equal<Vector<float>>([| 8.0; 16.0; 24.0 |], dst)

    [<Fact>]
    let ``With dst and src offset`` () =
        let dst = [| 0.0; 10.0; 20.0; 30.0; 0.0 |]
        let src = [| 0.0; 1.0; 2.0; 3.0; 0.0 |]
        LinearAlgebra.subScaledRowInPlace 2.0 1 1 3 dst src
        Assert.Equal<Vector<float>>([| 0.0; 8.0; 16.0; 24.0; 0.0 |], dst)

    [<Fact>]
    let ``Single element (scalar fallback)`` () =
        let dst = [| 42.0 |]
        let src = [| 2.0 |]
        LinearAlgebra.subScaledRowInPlace 3.0 0 0 1 dst src
        Assert.Equal<Vector<float>>([| 36.0 |], dst)

    [<Fact>]
    let ``No modification outside range`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 0.0; 1.0; 1.0; 0.0 |]
        LinearAlgebra.subScaledRowInPlace 2.0 1 1 2 dst src
        Assert.Equal<Vector<float>>([| 1.0; 0.0; 1.0; 4.0 |], dst)

    [<Fact>]
    let ``basic 3-element row subtraction`` () =
        let dst = [| 1.0; 2.0; 3.0 |]
        let src = [| 4.0; 5.0; 6.0 |]
        let alpha = 2.0
        LinearAlgebra.subScaledRowInPlace alpha 0 0 3 dst src
        Assert.Equal<Vector<float>>([| -7.0; -8.0; -9.0 |], dst)

    [<Fact>]
    let ``offsets respected correctly`` () =
        let dst = [| 0.0; 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 0.0; 0.0; 1.0; 2.0; 3.0 |]
        let alpha = 2.0
        LinearAlgebra.subScaledRowInPlace alpha 2 2 3 dst src
        Assert.Equal<Vector<float>>([| 0.0; 1.0; 0.0; -1.0; -2.0 |], dst)



module QrDecompositionTests =

    [<Fact>]
    let ``Decompose identity matrix`` () =
        let A = Matrix.identity 3
        let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.high Q I "Q should be identity"
        floatMatrixClose Accuracy.high R I "R should be identity"
        

    [<Fact>]
    let ``Reconstruct original matrix from Q * R`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                                1.0; 1.0;
                                0.0; 1.0 |])
        let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
        let QR = Matrix.matmul Q R
        floatMatrixClose Accuracy.high QR A "Q * R should reconstruct A"

    [<Fact>]
    let ``Q has orthonormal columns`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                                1.0; 1.0;
                                0.0; 1.0 |])
        let Q, _ = LinearAlgebra.qrModifiedGramSchmidt A
        let QTQ = Matrix.matmul (Matrix.transpose Q) Q
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.high QTQ I "Q^TQ should be identity"

    [<Fact>]
    let ``R is upper triangular`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                                1.0; 1.0;
                                0.0; 1.0 |])
        let _, R = LinearAlgebra.qrModifiedGramSchmidt A
        for i in 0 .. R.NumRows - 1 do
            for j in 0 .. i - 1 do
                floatClose Accuracy.high R.[i, j] 0.0 $"R[{i},{j}] should be zero"

    [<Fact>]
    let ``Tall matrix 4x2 decomposition`` () =
        let A = Matrix(4, 2, [| 1.0; 2.0;
                                3.0; 4.0;
                                5.0; 6.0;
                                7.0; 8.0 |])
        let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
        let QR = Matrix.matmul Q R
        floatMatrixClose Accuracy.medium QR A "QR ~ A for tall matrix"

    [<Fact>]
    let ``Wide matrix throws or truncates`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                                4.0; 5.0; 6.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.qrModifiedGramSchmidt A |> ignore)


module BackSubstituteTests =

    [<Fact>]
    let ``Solve simple upper triangular system`` () =
        let r = Matrix(2, 2, [| 2.0; 1.0;
                               0.0; 3.0 |])
        let y = [| 5.0; 6.0 |]
        let x = LinearAlgebra.backSubstitute r y
        let expected = [| 1.5; 2.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``Identity matrix returns y as solution`` () =
        let r = Matrix.identity 3
        let y = [| 4.0; -2.0; 0.5 |]
        let x = LinearAlgebra.backSubstitute r y
        Assert.Equal<Vector<float>>(y, x)

    [<Fact>]
    let ``Upper triangular with zeros below diagonal`` () =
        let r = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               0.0; 4.0; 5.0;
                               0.0; 0.0; 6.0 |])
        let y = [| 14.0; 23.0; 18.0 |]
        let x = LinearAlgebra.backSubstitute r y
        let expected = [| 1.0; 2.0; 3.0 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``Throws on non-square R`` () =
        let r = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               0.0; 4.0; 5.0 |])
        let y = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.backSubstitute r y |> ignore)

    [<Fact>]
    let ``Throws on mismatched dimensions with y`` () =
        let r = Matrix.identity 3
        let y = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.backSubstitute r y |> ignore)

    [<Fact>]
    let ``Throws on zero diagonal entry (division by zero)`` () =
        let r = Matrix(2, 2, [| 0.0; 1.0;
                               0.0; 2.0 |])
        let y = [| 3.0; 4.0 |]
        throws<DivideByZeroException>(fun () -> LinearAlgebra.backSubstitute r y |> ignore)



module SolveTriangularLinearSystemTests =


    [<Fact>]
    let ``Solve L x = b (lower triangular)`` () =
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0;
                               3.0; 4.0; 1.0 |])
        let b = [| 2.0; 5.0; 20.0 |]
        let x = LinearAlgebra.solveTriangularLinearSystem L b true
        let expected = [| 2.0; 1.0; 10.0 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``Solve L^T x = b (upper triangular)`` () =
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0;
                               3.0; 4.0; 1.0 |])
        let LT = Matrix.transpose L
        let b = [| 23.0; 9.0; 1.0 |]
        let x = LinearAlgebra.solveTriangularLinearSystem LT b false
        let expected = [| 10.0; 5.0; 1.0 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``Throws on mismatched dimensions`` () =
        let L = Matrix.identity 3
        let b = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystem L b true |> ignore)

    [<Fact>]
    let ``Throws on zero diagonal (division by zero)`` () =
        let L = Matrix(2, 2, [| 0.0; 0.0;
                               1.0; 1.0 |])
        let b = [| 1.0; 2.0 |]
        throws<DivideByZeroException>(fun () -> LinearAlgebra.solveTriangularLinearSystem L b true |> ignore)



module QrDecompositionHouseHolderTests =


    [<Fact>]
    let ``Q * R reconstructs A`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let Q, R = LinearAlgebra.qrDecompose A
        let QR = Matrix.matmul Q R
        floatMatrixClose Accuracy.high QR A "Q * R should reconstruct A"

    [<Fact>]
    let ``Q is orthogonal: Q^T Q = I`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                               1.0; 1.0;
                               1.0; 2.0 |])
        let Q, _ = LinearAlgebra.qrDecompose A
        let QtQ = Matrix.matmul (Matrix.transpose Q) Q
        let I = Matrix.identity Q.NumCols
        floatMatrixClose Accuracy.high QtQ I "Q^T Q should be identity"

    [<Fact>]
    let ``R is upper triangular`` () =
        let A = Matrix(3, 2, [| 1.0; 0.0;
                               2.0; 1.0;
                               3.0; 2.0 |])
        let _, R = LinearAlgebra.qrDecompose A
        for i = 0 to R.NumRows - 1 do
            for j = 0 to i - 1 do
                floatEqual R.[i, j] 0.0 1e-12

    [<Fact>]
    let ``QR of square matrix`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0 |])
        let Q, R = LinearAlgebra.qrDecompose A
        let QR = Q * R
        floatMatrixClose Accuracy.low QR A "QR should reconstruct A"

    [<Fact>]
    let ``QR of small square matrix`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let Q, R = LinearAlgebra.qrDecompose A
        let QR = Q * R
        floatMatrixClose Accuracy.low QR A "QR should reconstruct A"



module LeastSquaresQRTests =

    [<Fact>]
    let ``Overdetermined 3x2 system`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let b = [| 6.0; 0.0; 0.0 |]
        let x = LinearAlgebra.leastSquares A b
        let expected = [| 8.0; -3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-6

    [<Fact>]
    let ``Underdetermined 2x3 system (minimum norm)`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 0.0;
                               0.0; 1.0; 1.0 |])
        let b = [| 3.0; 2.0 |]
        let x = LinearAlgebra.leastSquares A b
        let expected = [| 0.33333333; 1.33333333; 0.66666667 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] x.[i] 1e-6

    [<Fact>]
    let ``Square system returns exact solution`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let x = LinearAlgebra.leastSquares A b
        let expected = [| 1.0; 3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``Residual Ax ~ projection of b`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let b = [| 6.0; 0.0; 0.0 |]
        let x = LinearAlgebra.leastSquares A b
        let Ax = Matrix.muliplyVector A x
        let expected = [| 5.0; 2.0; -1.0 |]
        for i in 0 .. 2 do
            floatEqual expected.[i] Ax.[i] 1e-6


module CholeskyTests =


    [<Fact>]
    let ``Decompose identity matrix`` () =
        let I = Matrix.identity 3
        let L = LinearAlgebra.cholesky I
        floatMatrixClose Accuracy.high L I "L should be identity for identity input"

    [<Fact>]
    let ``Decompose known positive-definite matrix`` () =
        let A = Matrix(2, 2, [| 4.0; 2.0;
                               2.0; 3.0 |])
        let L = LinearAlgebra.cholesky A
        let expected = Matrix(2, 2, [| 2.0; 0.0;
                                      1.0; sqrt 2.0 |])
        for i in 0 .. 1 do
            for j in 0 .. 1 do
                floatEqual expected.[i,j] L.[i,j] 1e-5

    [<Fact>]
    let ``Reconstruct A from L * L^T`` () =
        let A = Matrix(3, 3, [| 25.0; 15.0; -5.0;
                               15.0; 18.0;  0.0;
                               -5.0; 0.0; 11.0 |])
        let L = LinearAlgebra.cholesky A
        let LT = Matrix.transpose L
        let reconstructed = Matrix.matmul L LT
        floatMatrixClose Accuracy.high reconstructed A "Reconstructed A should match original" 

    [<Fact>]
    let ``Throws on non-positive-definite matrix`` () =
        let A = Matrix(2, 2, [| 0.0; 1.0;
                               1.0; 0.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.cholesky A |> ignore)

    [<Fact>]
    let ``Throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               2.0; 5.0; 6.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.cholesky A |> ignore)

    [<Fact>]
    let ``Zero matrix throws (not positive-definite)`` () =
        let A : Matrix<float> = Matrix.zeroCreate 3 3
        throws<ArgumentException>(fun () -> LinearAlgebra.cholesky A |> ignore)


module LeastSquaresCholeskyTests =

    [<Fact>]
    let ``Solve overdetermined 3x2 system`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let b = [| 6.0; 0.0; 0.0 |]
        let beta = LinearAlgebra.leastSquaresCholesky A b
        let expected = [| 8.0; -3.0 |]
        for i in 0..1 do
            floatEqual expected.[i] beta.[i] 1e-6

    [<Fact>]
    let ``Reconstruction A * beta ~ projection of b`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let b = [| 6.0; 0.0; 0.0 |]
        let beta = LinearAlgebra.leastSquaresCholesky A b
        let predicted = Matrix.muliplyVector A beta
        let expected = [| 5.0; 2.0; -1.0 |]
        for i in 0..2 do
            floatEqual expected.[i] predicted.[i] 1e-6

    [<Fact>]
    let ``Throws on dimension mismatch A and b`` () =
        let A = Matrix.identity 3
        let b = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.leastSquaresCholesky A b |> ignore)

    [<Fact>]
    let ``Exact solution when A is square and full-rank`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let beta = LinearAlgebra.leastSquaresCholesky A b
        let expected = [| 1.0; 3.0 |]
        for i in 0..1 do
            floatEqual expected.[i] beta.[i] 1e-10
