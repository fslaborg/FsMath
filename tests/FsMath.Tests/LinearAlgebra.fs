namespace FsMath.Tests.LinearAlgebraTests

open Xunit
open System    
open FsMath
open FsMath.SpecialFunctions

open FsMath.Tests.ExpectoStyle

module LinearAlgebra =


    // Assuming we have the following signature available:
    // Matrix<'T> type with .NumRows, .NumCols, .Data, etc.,
    // and a static member solveTriangularLinearSystem.

    ///// Simple matrix multiplication: A (n×k) * B (k×m) -> C (n×m).
    ///// For verifying K * X == B.
    //let matrixMultiply (A: Matrix<float>) (B: Matrix<float>) : Matrix<float> =
    //    if A.NumCols <> B.NumRows then
    //        invalidArg (nameof B) "Matrix dimensions are incompatible for multiplication."
    //    let n, k = A.NumRows, A.NumCols
    //    let k2, m = B.NumRows, B.NumCols
    //    let dataC = Array.zeroCreate<float> (n*m)
    //    let Aarr, Barr = A.Data, B.Data

    //    for i in 0 .. n-1 do
    //        for j in 0 .. m-1 do
    //            let mutable sum = 0.0
    //            for t in 0 .. k-1 do
    //                sum <- sum + Aarr.[i*k + t] * Barr.[t*m + j]
    //            dataC.[i*m + j] <- sum
    
    //    Matrix<float>(n, m, dataC)

    /// Approximate check for floating-point equality
    let approxEqual (x: float) (y: float) (tol: float) =
        abs (x - y) <= tol

    [<Fact>]
    let ``Solve Triangular - Lower`` () =
        // 1) Define a 3×3 lower-triangular matrix K
        //    For example:
        //      [ [ 2.0,  0.0,  0.0 ]
        //        [ 4.0,  5.0,  0.0 ]
        //        [ 1.0,  2.0,  3.0 ] ]
        let Karr = [|
            2.0; 0.0; 0.0;
            4.0; 5.0; 0.0;
            1.0; 2.0; 3.0
        |]
        let K = Matrix<float>(3,3,Karr)

        // 2) Define a 3×2 matrix B
        let Barr = [|
            10.0;  20.0;
            14.0;  28.0;
            13.0;  14.0;
        |]
        let Bmat = Matrix<float>(3,2,Barr)

        // 3) Solve K * X = B
        let X = LinearAlgebra.solveTriangularLinearSystem K Bmat true

        // 4) Check that K*X ~ B
        let product = Matrix.multiply K X

        for i in 0 .. product.NumRows - 1 do
            for j in 0 .. product.NumCols - 1 do
                let expected = Bmat.Data.[i*Bmat.NumCols + j]
                let actual   = product.Data.[i*product.NumCols + j]
                Assert.True(approxEqual expected actual 1e-8, 
                            sprintf "Mismatch at (%d, %d): expected=%f, got=%f" i j expected actual)

    [<Fact>]
    let ``Solve Triangular - Upper`` () =
        // 1) Define a 3×3 upper-triangular matrix K
        //      [ [ 2.0,  1.0,   3.0 ]
        //        [ 0.0,  5.0,   2.0 ]
        //        [ 0.0,  0.0,   4.0 ] ]
        let Karr = [|
            2.0; 1.0; 3.0;
            0.0; 5.0; 2.0;
            0.0; 0.0; 4.0
        |]
        let K = Matrix<float>(3,3,Karr)

        // 2) Define a 3×2 matrix B
        let Barr = [|
            10.0;  20.0;
             5.0;  25.0;
            16.0;   8.0
        |]
        let Bmat = Matrix<float>(3,2,Barr)

        // 3) Solve K * X = B
        let X = LinearAlgebra.solveTriangularLinearSystem K Bmat false

        // 4) Check that K*X ~ B
        let product = Matrix.multiply K X

        for i in 0 .. product.NumRows - 1 do
            for j in 0 .. product.NumCols - 1 do
                let expected = Bmat.Data.[i*Bmat.NumCols + j]
                let actual   = product.Data.[i*product.NumCols + j]
                Assert.True(approxEqual expected actual 1e-8,
                            sprintf "Mismatch at (%d, %d): expected=%f, got=%f" i j expected actual)


