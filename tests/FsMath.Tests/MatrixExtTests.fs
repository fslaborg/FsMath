namespace FsMath.Tests.MatrixExtTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle

// Tests for MatrixExt.Inverse() extension method using quotation evaluation
// This ensures inline functions are tracked by coverage tools (F# 8.0+)
module MatrixExtInverseTests =

    open Microsoft.FSharp.Linq.RuntimeHelpers

    // Helper to evaluate quotations
    let eval q = LeafExpressionConverter.EvaluateQuotation q

    [<Fact>]
    let ``Inverse of identity matrix is identity`` () =
        let I = Matrix.identity 3
        let inv = eval <@ I.Inverse() @> :?> Matrix<float>
        floatMatrixClose Accuracy.high inv I "Inverse of identity should be identity"

    [<Fact>]
    let ``Inverse of 2x2 diagonal matrix`` () =
        let A = Matrix(2, 2, [| 2.0; 0.0;
                               0.0; 4.0 |])
        let inv = eval <@ A.Inverse() @> :?> Matrix<float>
        let expected = Matrix(2, 2, [| 0.5; 0.0;
                                      0.0; 0.25 |])
        floatMatrixClose Accuracy.high inv expected "Inverse of diagonal matrix"

    [<Fact>]
    let ``Inverse of simple 2x2 matrix`` () =
        let A = Matrix(2, 2, [| 4.0; 7.0;
                               2.0; 6.0 |])
        let inv = eval <@ A.Inverse() @> :?> Matrix<float>
        // A^-1 = (1/det(A)) * [6 -7; -2 4], det(A) = 10
        let expected = Matrix(2, 2, [| 0.6; -0.7;
                                      -0.2; 0.4 |])
        floatMatrixClose Accuracy.medium inv expected "Inverse of 2x2 matrix"

    [<Fact>]
    let ``A * A^-1 equals identity`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               0.0; 1.0; 4.0;
                               5.0; 6.0; 0.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "A * A^-1 should be identity"

    [<Fact>]
    let ``A^-1 * A equals identity`` () =
        let A = Matrix(3, 3, [| 2.0; 1.0; 1.0;
                               1.0; 2.0; 1.0;
                               1.0; 1.0; 2.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul Ainv A
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "A^-1 * A should be identity"

    [<Fact>]
    let ``Inverse of 3x3 matrix with known result`` () =
        let A = Matrix(3, 3, [| 1.0; 0.0; 2.0;
                               2.0; 1.0; 0.0;
                               0.0; 1.0; 1.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        // Verify by reconstruction
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "Verify inverse through A*A^-1"

    [<Fact>]
    let ``Inverse of inverse returns original`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 1.0;
                               0.0; 1.0; 2.0;
                               1.0; 0.0; 1.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let AinvInv = eval <@ Ainv.Inverse() @> :?> Matrix<float>
        floatMatrixClose Accuracy.low AinvInv A "(A^-1)^-1 should equal A"

    [<Fact>]
    let ``Inverse of 4x4 matrix`` () =
        let A = Matrix(4, 4, [| 1.0; 0.0; 0.0; 0.0;
                               0.0; 2.0; 0.0; 0.0;
                               0.0; 0.0; 3.0; 0.0;
                               0.0; 0.0; 0.0; 4.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let expected = Matrix(4, 4, [| 1.0; 0.0; 0.0; 0.0;
                                      0.0; 0.5; 0.0; 0.0;
                                      0.0; 0.0; 1.0/3.0; 0.0;
                                      0.0; 0.0; 0.0; 0.25 |])
        floatMatrixClose Accuracy.high Ainv expected "Inverse of 4x4 diagonal matrix"

    [<Fact>]
    let ``Inverse of upper triangular matrix`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               0.0; 4.0; 5.0;
                               0.0; 0.0; 6.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "Inverse of upper triangular matrix"

    [<Fact>]
    let ``Inverse of lower triangular matrix`` () =
        let A = Matrix(3, 3, [| 2.0; 0.0; 0.0;
                               3.0; 4.0; 0.0;
                               5.0; 6.0; 7.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "Inverse of lower triangular matrix"

    [<Fact>]
    let ``Inverse preserves determinant relationship`` () =
        let A = Matrix(2, 2, [| 3.0; 2.0;
                               1.0; 4.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let detA = LinearAlgebra.determinant A
        let detAinv = LinearAlgebra.determinant Ainv
        // det(A^-1) = 1/det(A)
        floatClose Accuracy.medium (detA * detAinv) 1.0 "det(A) * det(A^-1) = 1"

    [<Fact>]
    let ``Inverse with positive and negative elements`` () =
        let A = Matrix(3, 3, [| 2.0; -1.0; 0.0;
                               -1.0; 2.0; -1.0;
                               0.0; -1.0; 2.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "Inverse with mixed signs"

    [<Fact>]
    let ``Throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let action = fun () ->
            let _ = eval <@ A.Inverse() @> :?> Matrix<float>
            ()
        throws<ArgumentException>(action)

    [<Fact>]
    let ``Inverse of small values maintains precision`` () =
        let scale = 0.01
        let A = Matrix(2, 2, [| 4.0*scale; 7.0*scale;
                               2.0*scale; 6.0*scale |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.medium product I "Inverse with small values"

    [<Fact>]
    let ``Inverse of large values maintains precision`` () =
        let scale = 100.0
        let A = Matrix(2, 2, [| 4.0*scale; 7.0*scale;
                               2.0*scale; 6.0*scale |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 2
        floatMatrixClose Accuracy.medium product I "Inverse with large values"

    [<Fact>]
    let ``Inverse of symmetric matrix`` () =
        let A = Matrix(3, 3, [| 4.0; 1.0; 2.0;
                               1.0; 5.0; 3.0;
                               2.0; 3.0; 6.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.medium product I "Inverse of symmetric matrix"

    [<Fact>]
    let ``Inverse of 1x1 matrix`` () =
        let A = Matrix(1, 1, [| 5.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let expected = Matrix(1, 1, [| 0.2 |])
        floatMatrixClose Accuracy.high Ainv expected "Inverse of 1x1 matrix"

    [<Fact>]
    let ``Inverse commutes with transpose`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 1.0;
                               0.0; 1.0; 2.0;
                               1.0; 0.0; 1.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let AT = Matrix.transpose A
        let ATinv = eval <@ AT.Inverse() @> :?> Matrix<float>
        let AinvT = Matrix.transpose Ainv
        floatMatrixClose Accuracy.low ATinv AinvT "(A^T)^-1 = (A^-1)^T"

    [<Fact>]
    let ``Inverse of well-conditioned matrix`` () =
        let A = Matrix(3, 3, [| 10.0; 1.0; 0.0;
                               1.0; 10.0; 1.0;
                               0.0; 1.0; 10.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let product = Matrix.matmul A Ainv
        let I = Matrix.identity 3
        floatMatrixClose Accuracy.high product I "Inverse of well-conditioned matrix"

    [<Fact>]
    let ``Inverse with integer-like values`` () =
        let A = Matrix(3, 3, [| 3.0; 0.0; 0.0;
                               0.0; 2.0; 0.0;
                               0.0; 0.0; 5.0 |])
        let Ainv = eval <@ A.Inverse() @> :?> Matrix<float>
        let expected = Matrix(3, 3, [| 1.0/3.0; 0.0; 0.0;
                                      0.0; 0.5; 0.0;
                                      0.0; 0.0; 0.2 |])
        floatMatrixClose Accuracy.high Ainv expected "Inverse with integer-like values"
