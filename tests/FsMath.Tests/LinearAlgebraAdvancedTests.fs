namespace FsMath.Tests.LinearAlgebraAdvancedTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle


module SolveTriangularLinearSystemsTests =
    // Tests for the matrix version (K * X = B)

    [<Fact>]
    let ``Solve L * X = B (lower triangular, multiple RHS)`` () =
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 1.0; 0.0;
                               3.0; 4.0; 1.0 |])
        let B = Matrix(3, 2, [| 2.0; 4.0;
                               5.0; 9.0;
                               20.0; 34.0 |])
        let X = LinearAlgebra.solveTriangularLinearSystems L B true
        let expected = Matrix(3, 2, [| 2.0; 4.0;
                                      1.0; 1.0;
                                      10.0; 18.0 |])
        floatMatrixClose Accuracy.high X expected "X should match expected solution"

    [<Fact>]
    let ``Solve U * X = B (upper triangular, multiple RHS)`` () =
        let U = Matrix(2, 2, [| 2.0; 1.0;
                               0.0; 3.0 |])
        let B = Matrix(2, 2, [| 5.0; 7.0;
                               6.0; 9.0 |])
        let X = LinearAlgebra.solveTriangularLinearSystems U B false
        let expected = Matrix(2, 2, [| 1.5; 2.0;
                                      2.0; 3.0 |])
        floatMatrixClose Accuracy.high X expected "X should match expected solution"

    [<Fact>]
    let ``Solve with identity matrix returns B`` () =
        let I = Matrix.identity 3
        let B = Matrix(3, 2, [| 1.0; 2.0;
                               3.0; 4.0;
                               5.0; 6.0 |])
        let X = LinearAlgebra.solveTriangularLinearSystems I B true
        floatMatrixClose Accuracy.high X B "Identity matrix should return B"

    [<Fact>]
    let ``Throws on non-square K`` () =
        let K = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               0.0; 1.0; 2.0 |])
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystems K B true |> ignore)

    [<Fact>]
    let ``Throws on mismatched dimensions K and B`` () =
        let K = Matrix.identity 3
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystems K B true |> ignore)


module LUDecompositionTests =

    [<Fact>]
    let ``Decompose 2x2 matrix`` () =
        let A = Matrix(2, 2, [| 4.0; 3.0;
                               6.0; 3.0 |])
        let (P, L, U) = LinearAlgebra.luDecompose A
        // Reconstruct PA = LU
        let PA = Matrix.permuteRowsBy P A
        let LU = Matrix.matmul L U
        floatMatrixClose Accuracy.high PA LU "P*A should equal L*U"

    [<Fact>]
    let ``Decompose 3x3 matrix`` () =
        let A = Matrix(3, 3, [| 2.0; 1.0; 1.0;
                               4.0; -6.0; 0.0;
                               -2.0; 7.0; 2.0 |])
        let (P, L, U) = LinearAlgebra.luDecompose A
        let PA = Matrix.permuteRowsBy P A
        let LU = Matrix.matmul L U
        floatMatrixClose Accuracy.medium PA LU "P*A should equal L*U"

    [<Fact>]
    let ``L has diagonal ones`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let (_, L, _) = LinearAlgebra.luDecompose A
        for i in 0 .. 2 do
            floatEqual L.[i,i] 1.0 1e-10

    [<Fact>]
    let ``U is upper triangular`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 10.0 |])
        let (_, _, U) = LinearAlgebra.luDecompose A
        for i in 1 .. 2 do
            for j in 0 .. i - 1 do
                floatEqual U.[i,j] 0.0 1e-10

    [<Fact>]
    let ``Identity matrix decomposition`` () =
        let I = Matrix.identity 3
        let (P, L, U) = LinearAlgebra.luDecompose I
        floatMatrixClose Accuracy.high L I "L should be identity"
        floatMatrixClose Accuracy.high U I "U should be identity"

    [<Fact>]
    let ``Throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.luDecompose A |> ignore)


module SolveLinearSystemLUTests =

    [<Fact>]
    let ``Solve 2x2 system`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let x = LinearAlgebra.solveLinearSystem A b
        let expected = [| 1.0; 3.0 |]
        for i in 0 .. 1 do
            floatEqual expected.[i] x.[i] 1e-10

    [<Fact>]
    let ``Solve 3x3 system`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               2.0; 5.0; 3.0;
                               1.0; 0.0; 8.0 |])
        let b = [| 14.0; 18.0; 27.0 |]
        let x = LinearAlgebra.solveLinearSystem A b
        // Verify A*x = b
        let Ax = Matrix.muliplyVector A x
        for i in 0 .. 2 do
            floatEqual Ax.[i] b.[i] 1e-10

    [<Fact>]
    let ``Solve with identity matrix`` () =
        let I = Matrix.identity 3
        let b = [| 1.0; 2.0; 3.0 |]
        let x = LinearAlgebra.solveLinearSystem I b
        Assert.Equal<Vector<float>>(b, x)

    [<Fact>]
    let ``Throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let b = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystem A b |> ignore)

    [<Fact>]
    let ``Throws on dimension mismatch A and b`` () =
        let A = Matrix.identity 3
        let b = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystem A b |> ignore)


module SolveLinearSystemsLUTests =

    [<Fact>]
    let ``Solve A * X = B with 2x2 matrices`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let B = Matrix(2, 2, [| 5.0; 8.0;
                               10.0; 11.0 |])
        let X = LinearAlgebra.solveLinearSystems A B
        // Verify A*X = B reconstruction instead of checking exact values
        let AX = Matrix.matmul A X
        floatMatrixClose Accuracy.high AX B "A*X should reconstruct B"

    [<Fact>]
    let ``Verify A * X = B reconstruction`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               2.0; 5.0; 3.0;
                               1.0; 0.0; 8.0 |])
        let B = Matrix(3, 2, [| 14.0; 20.0;
                               18.0; 31.0;
                               27.0; 25.0 |])
        let X = LinearAlgebra.solveLinearSystems A B
        let AX = Matrix.matmul A X
        floatMatrixClose Accuracy.medium AX B "A*X should reconstruct B"

    [<Fact>]
    let ``Solve with identity matrix returns B`` () =
        let I = Matrix.identity 3
        let B = Matrix(3, 2, [| 1.0; 2.0;
                               3.0; 4.0;
                               5.0; 6.0 |])
        let X = LinearAlgebra.solveLinearSystems I B
        floatMatrixClose Accuracy.high X B "Identity matrix should return B"

    [<Fact>]
    let ``Throws on non-square A`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystems A B |> ignore)

    [<Fact>]
    let ``Throws on dimension mismatch A and B`` () =
        let A = Matrix.identity 3
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystems A B |> ignore)


module InverseTests =

    [<Fact>]
    let ``Inverse of 2x2 matrix`` () =
        let A = Matrix(2, 2, [| 4.0; 7.0;
                               2.0; 6.0 |])
        let Ainv = LinearAlgebra.inverse A
        let I = Matrix.identity 2
        let AAinv = Matrix.matmul A Ainv
        floatMatrixClose Accuracy.medium AAinv I "A * A^-1 should be identity"

    [<Fact>]
    let ``Inverse of 3x3 matrix`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               2.0; 5.0; 3.0;
                               1.0; 0.0; 8.0 |])
        let Ainv = LinearAlgebra.inverse A
        let I = Matrix.identity 3
        let AAinv = Matrix.matmul A Ainv
        floatMatrixClose Accuracy.medium AAinv I "A * A^-1 should be identity"

    [<Fact>]
    let ``Inverse of identity is identity`` () =
        let I = Matrix.identity 3
        let Iinv = LinearAlgebra.inverse I
        floatMatrixClose Accuracy.high Iinv I "Inverse of identity is identity"

    [<Fact>]
    let ``Inverse is involution: (A^-1)^-1 = A`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let Ainv = LinearAlgebra.inverse A
        let A_again = LinearAlgebra.inverse Ainv
        floatMatrixClose Accuracy.medium A_again A "(A^-1)^-1 should equal A"

    [<Fact>]
    let ``Throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.inverse A |> ignore)


module SolveLinearQRTests =

    [<Fact>]
    let ``Solve 2x2 system with QR`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let b = [| 5.0; 10.0 |]
        let x = LinearAlgebra.solveLinearQR A b
        // Verify A*x = b
        let Ax = Matrix.muliplyVector A x
        floatEqual Ax.[0] b.[0] 1e-10
        floatEqual Ax.[1] b.[1] 1e-10

    [<Fact>]
    let ``Solve 3x3 system with QR`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               2.0; 5.0; 3.0;
                               1.0; 0.0; 8.0 |])
        let b = [| 14.0; 18.0; 27.0 |]
        let x = LinearAlgebra.solveLinearQR A b
        // Verify A*x = b
        let Ax = Matrix.muliplyVector A x
        for i in 0 .. 2 do
            floatEqual Ax.[i] b.[i] 1e-9

    [<Fact>]
    let ``Solve with identity matrix returns b`` () =
        let I = Matrix.identity 3
        let b = [| 1.0; 2.0; 3.0 |]
        let x = LinearAlgebra.solveLinearQR I b
        floatArrayClose b x 1e-10

    [<Fact>]
    let ``Throws on dimension mismatch A and b`` () =
        let A = Matrix.identity 3
        let b = [| 1.0; 2.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearQR A b |> ignore)

    [<Fact>]
    let ``Solve simple diagonal system`` () =
        let A = Matrix(3, 3, [| 2.0; 0.0; 0.0;
                               0.0; 3.0; 0.0;
                               0.0; 0.0; 4.0 |])
        let b = [| 6.0; 9.0; 12.0 |]
        let x = LinearAlgebra.solveLinearQR A b
        let expected = [| 3.0; 3.0; 3.0 |]
        floatArrayClose expected x 1e-10

    [<Fact>]
    let ``Solve with negative values`` () =
        let A = Matrix(2, 2, [| -2.0; 1.0;
                               1.0; -3.0 |])
        let b = [| 3.0; -5.0 |]
        let x = LinearAlgebra.solveLinearQR A b
        // Verify A*x = b
        let Ax = Matrix.muliplyVector A x
        floatEqual Ax.[0] b.[0] 1e-10
        floatEqual Ax.[1] b.[1] 1e-10


module DeterminantTests =

    [<Fact>]
    let ``Determinant of identity is 1`` () =
        let I = Matrix.identity 3
        let det = LinearAlgebra.determinant I
        floatEqual det 1.0 1e-10

    [<Fact>]
    let ``Determinant of 2x2 matrix`` () =
        let A = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        let det = LinearAlgebra.determinant A
        // det = 1*4 - 2*3 = -2
        floatEqual det -2.0 1e-10

    [<Fact>]
    let ``Determinant of 3x3 matrix`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               0.0; 1.0; 4.0;
                               5.0; 6.0; 0.0 |])
        let det = LinearAlgebra.determinant A
        // Can verify with manual calculation or reference
        floatEqual det 1.0 1e-10

    [<Fact>]
    let ``Determinant of singular matrix is zero`` () =
        let A = Matrix(2, 2, [| 1.0; 2.0;
                               2.0; 4.0 |])
        let det = LinearAlgebra.determinant A
        floatEqual det 0.0 1e-10

    [<Fact>]
    let ``Throws on non-square matrix`` () =
        let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.determinant A |> ignore)


module PseudoInverseTests =

    [<Fact>]
    let ``Pseudo-inverse of overdetermined system (tall matrix)`` () =
        let A = Matrix(4, 2, [| 1.0; 0.0;
                               0.0; 1.0;
                               1.0; 0.0;
                               0.0; 1.0 |])
        let Aplus = LinearAlgebra.pseudoInvers A
        // Aplus should be 2x4
        Assert.Equal(2, Aplus.NumRows)
        Assert.Equal(4, Aplus.NumCols)
        // A * Aplus * A should approximately equal A
        let reconstruction = Matrix.matmul (Matrix.matmul A Aplus) A
        floatMatrixClose Accuracy.medium reconstruction A "A*A+*A should approximate A"

    [<Fact>]
    let ``Pseudo-inverse of underdetermined system (wide matrix)`` () =
        let A = Matrix(2, 4, [| 1.0; 0.0; 1.0; 0.0;
                               0.0; 1.0; 0.0; 1.0 |])
        let Aplus = LinearAlgebra.pseudoInvers A
        // Aplus should be 4x2
        Assert.Equal(4, Aplus.NumRows)
        Assert.Equal(2, Aplus.NumCols)
        // A * Aplus * A should approximately equal A
        let reconstruction = Matrix.matmul (Matrix.matmul A Aplus) A
        floatMatrixClose Accuracy.low reconstruction A "A*A+*A should approximate A"

    [<Fact>]
    let ``Pseudo-inverse of square invertible matrix is regular inverse`` () =
        let A = Matrix(2, 2, [| 2.0; 1.0;
                               1.0; 3.0 |])
        let Aplus = LinearAlgebra.pseudoInvers A
        let Ainv = LinearAlgebra.inverse A
        floatMatrixClose Accuracy.medium Aplus Ainv "Pseudo-inverse of invertible matrix equals inverse"

    [<Fact>]
    let ``Pseudo-inverse of identity is identity`` () =
        let I = Matrix.identity 3
        let Iplus = LinearAlgebra.pseudoInvers I
        floatMatrixClose Accuracy.high Iplus I "Pseudo-inverse of identity is identity"


module HatMatrixTests =

    [<Fact>]
    let ``Hat matrix is idempotent: H*H = H`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let H = LinearAlgebra.hatMatrix X
        let HH = Matrix.matmul H H
        floatMatrixClose Accuracy.medium HH H "H*H should equal H"

    [<Fact>]
    let ``Hat matrix is symmetric`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let H = LinearAlgebra.hatMatrix X
        let HT = Matrix.transpose H
        floatMatrixClose Accuracy.medium H HT "H should equal H^T"

    [<Fact>]
    let ``Hat matrix has correct dimensions`` () =
        let X = Matrix(5, 3, [| 1.0; 1.0; 1.0;
                               1.0; 2.0; 4.0;
                               1.0; 3.0; 9.0;
                               1.0; 4.0; 16.0;
                               1.0; 5.0; 25.0 |])
        let H = LinearAlgebra.hatMatrix X
        Assert.Equal(5, H.NumRows)
        Assert.Equal(5, H.NumCols)


module LeverageTests =

    [<Fact>]
    let ``Leverage from hat matrix`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let H = LinearAlgebra.hatMatrix X
        let lev = LinearAlgebra.leverageBy H
        Assert.Equal(4, lev.Length)
        // Leverages should sum to rank(X) = 2
        let sum = Array.sum lev
        floatEqual sum 2.0 1e-10

    [<Fact>]
    let ``Direct leverage computation`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let lev = LinearAlgebra.leverage X
        Assert.Equal(4, lev.Length)
        // Leverages should sum to rank(X) = 2
        let sum = Array.sum lev
        floatEqual sum 2.0 1e-10

    [<Fact>]
    let ``Leverage computed directly and from hat matrix match`` () =
        let X = Matrix(5, 3, [| 1.0; 1.0; 1.0;
                               1.0; 2.0; 4.0;
                               1.0; 3.0; 9.0;
                               1.0; 4.0; 16.0;
                               1.0; 5.0; 25.0 |])
        let H = LinearAlgebra.hatMatrix X
        let lev1 = LinearAlgebra.leverageBy H
        let lev2 = LinearAlgebra.leverage X
        for i in 0 .. 4 do
            floatEqual lev1.[i] lev2.[i] 1e-10

    [<Fact>]
    let ``All leverages are non-negative`` () =
        let X = Matrix(4, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0;
                               1.0; 4.0 |])
        let lev = LinearAlgebra.leverage X
        for i in 0 .. 3 do
            Assert.True(lev.[i] >= 0.0)
