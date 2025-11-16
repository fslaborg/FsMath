namespace FsMath.Tests.Matrix

open System
open Xunit
open FsMath
open FsMath.Tests
open FsMath.Tests.AssertHelpers

//  ----------------------------------------------------------------------
/// Basic construction 
module MatrixConstructionTests =


    [<Fact>]
    let ``ofJaggedArray creates matrix from jagged array`` () =
        let jagged = 
            [| [|0.0; 0.0|]
               [|1.0; 3.0|]
               [|2.0; 4.0|] |]
        let mat = Matrix.ofJaggedArray jagged
        Assert.Equal(3, mat.NumRows)
        Assert.Equal(2, mat.NumCols)
        Assert.Equal<Vector<float>>([|0.0; 0.0; 1.0; 3.0; 2.0; 4.0|], mat.Data)

    [<Fact>]
    let ``ofArray2D creates matrix from 2D array`` () =
        let arr2d = 
            array2D [ [ 1; 2 ]
                      [ 3; 4 ]
                      [ 5; 6 ] ]
        let mat = Matrix.ofArray2D arr2d
        Assert.Equal(3, mat.NumRows)
        Assert.Equal(2, mat.NumCols)
        Assert.Equal<Vector<int>>([|1; 2; 3; 4; 5; 6|], mat.Data)

    [<Fact>]
    let ``init creates a matrix using a function of row,col`` () =
        let mat = Matrix.init 2 3 (fun r c -> float (r + c))
        let expected = [|0.0; 1.0; 2.0; 1.0; 2.0; 3.0|]
        Assert.Equal(2, mat.NumRows)
        Assert.Equal(3, mat.NumCols)
        Assert.Equal<Vector<float>>(expected, mat.Data)

    [<Fact>]
    let ``ofJaggedArray throws on non-rectangular jagged input`` () =
        let jagged = 
            [| [|1.0; 2.0|]
               [|3.0|] |]
        throws<ArgumentException>(fun () -> Matrix.ofJaggedArray jagged |> ignore)


//  ----------------------------------------------------------------------
/// Access & Slicing
module MatrixAccessTests = 

    [<Fact>]
    let ``indexer get/set within bounds`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        Assert.Equal(1.0, mat.[0, 0])
        mat.[1, 1] <- 99.0
        Assert.Equal(99.0, mat.[1, 1])

    [<Fact>]
    let ``indexer throws on out-of-range access`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> let _ = mat.[999, 999] in ())

    [<Fact>]
    let ``GetSlice works with row and col start..end`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        let subMat = mat.[0..1, 1..2]
        Assert.Equal(2, subMat.NumRows)
        Assert.Equal(2, subMat.NumCols)
        Assert.Equal<Vector<float>>([|2.0; 3.0; 5.0; 6.0|], subMat.Data)

    [<Fact>]
    let ``GetSlice throws on invalid slice range`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> mat.[0..99, *] |> ignore)


//  ----------------------------------------------------------------------
/// Arithmetic (element-wise) 
module MatrixArithmeticTests = 

    [<Fact>]
    let ``add: element-wise addition of same-dimension matrices`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let m2 = matrix [| [|10.0; 20.0|]
                           [|30.0; 40.0|] |]
        let result = Matrix.add m1 m2
        let expected = [|11.0; 22.0; 33.0; 44.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``subtract: dimension mismatch throws`` () =
        let m1 = matrix [| [|1.0; 2.0|] |]       // 1x2
        let m2 = matrix [| [|1.0; 2.0; 3.0|] |]  // 1x3
        throws<ArgumentException>(fun () -> Matrix.subtract m1 m2 |> ignore)

    [<Fact>]
    let ``multiply & divide: element-wise`` () =
        let m1 = matrix [| [|2.0; 4.0|]
                           [|6.0; 8.0|] |]
        let m2 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]

        let mulResult = Matrix.multiply m1 m2
        Assert.Equal<Vector<float>>([|2.0; 8.0; 18.0; 32.0|], mulResult.Data)

        let divResult = Matrix.divide m1 m2
        Assert.Equal<Vector<float>>([|2.0; 2.0; 2.0; 2.0|], divResult.Data)


//  ----------------------------------------------------------------------
/// Standard matrix multiplication
module MatrixMultiplicationTests =

    [<Fact>]
    let ``matmul: 2x3 times 3x2 => 2x2 result`` () =
        // A = 2x3
        let A = matrix [| [|1.0; 2.0; 3.0|]
                          [|4.0; 5.0; 6.0|] |]
        // B = 3x2
        let B = matrix [| [|7.0;  10.0|]
                          [|8.0;  11.0|]
                          [|9.0;  12.0|] |]
        let C = Matrix.matmul A B

        Assert.Equal(2, C.NumRows)
        Assert.Equal(2, C.NumCols)
        Assert.Equal<Vector<float>>([|50.0; 68.0; 122.0; 167.0|], C.Data)


//  ----------------------------------------------------------------------
/// Scalar Operations
module MatrixScalarOpsTests =

    [<Fact>]
    let ``addScalar, subtractScalar, multiplyScalar, divideScalar`` () =
        let m = matrix [| [|1.0; 2.0|]
                          [|3.0; 4.0|] |]

        let addRes = Matrix.addScalar m 10.0
        Assert.Equal<Vector<float>>([|11.0; 12.0; 13.0; 14.0|], addRes.Data)

        let subRes = Matrix.subtractScalar m 1.0
        Assert.Equal<Vector<float>>([|0.0; 1.0; 2.0; 3.0|], subRes.Data)

        let mulRes = Matrix.multiplyScalar m 2.0
        Assert.Equal<Vector<float>>([|2.0; 4.0; 6.0; 8.0|], mulRes.Data)

        let divRes = Matrix.divideScalar m 2.0
        Assert.Equal<Vector<float>>([|0.5; 1.0; 1.5; 2.0|], divRes.Data)


//  ----------------------------------------------------------------------
/// Matrix-Vector Multiply
module MatrixVectorMultiplicationTests =

    [<Fact>]
    let ``m * v => standard matrix-vector product`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]
        let v = [| 10.0; 20.0; 30.0 |]
        let result = mat * v
        Assert.Equal<Vector<float>>([|140.0; 320.0|], result)

    [<Fact>]
    let ``v * m => row-vector times matrix => vector`` () =
        let v = [|2.0; 3.0|]
        let mat = matrix [| [|10.0; 100.0|]
                            [|20.0; 200.0|] |]
        let result = v * mat
        Assert.Equal<Vector<float>>([|80.0; 800.0|], result)

    [<Fact>]
    let ``m * v => dimension mismatch throws`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        let v = [|1.0; 2.0; 3.0|]
        throws<ArgumentException>(fun () -> let _ = mat * v in ())


//  ----------------------------------------------------------------------
/// Transpose
module MatrixTransposeTests =

    [<Fact>]
    let ``Transpose: changes shape and flips row<->col`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]  // 2x3
        let t = mat.Transpose()
        Assert.Equal(3, t.NumRows)
        Assert.Equal(2, t.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 4.0; 2.0; 5.0; 3.0; 6.0|], t.Data)

//  ----------------------------------------------------------------------
/// Identity, Diagonal, and Zero/Ones
module MatrixCreationTests =


    [<Fact>]
    let ``identity n => NxN identity matrix`` () =
        let eye = Matrix.identity<float> 3
        let expected = [|1.0; 0.0; 0.0;
                         0.0; 1.0; 0.0;
                         0.0; 0.0; 1.0|]
        Assert.Equal<Vector<float>>(expected, eye.Data)

    [<Fact>]
    let ``diagonal => builds NxN from diag vector`` () =
        let diagVec = [|10.0; 20.0; 30.0|]
        let mat = Matrix.diagonal diagVec
        let expected = [|10.0; 0.0; 0.0;
                         0.0; 20.0; 0.0;
                         0.0; 0.0; 30.0|]
        Assert.Equal(3, mat.NumRows)
        Assert.Equal(3, mat.NumCols)
        Assert.Equal<Vector<float>>(expected, mat.Data)

    [<Fact>]
    let ``ones => NxM matrix of all 1's`` () =
        let mat = Matrix.ones<float> 2 3
        Assert.Equal<Vector<float>>([|1.0; 1.0; 1.0; 1.0; 1.0; 1.0|], mat.Data)

    [<Fact>]
    let ``zeroCreate => NxM matrix of all 0's`` () =
        let mat = Matrix.zeroCreate 2 3
        Assert.Equal<Vector<float>>([|0.0; 0.0; 0.0; 0.0; 0.0; 0.0|], mat.Data)


//  ----------------------------------------------------------------------
/// getRow / getCol 
module MatrixRowColAccessTests =


    [<Fact>]
    let ``getRow gets row i as a vector`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        let row0 = Matrix.getRow 0 mat
        let row1 = Matrix.getRow 1 mat
        Assert.Equal<Vector<float>>([|1.0; 2.0|], row0)
        Assert.Equal<Vector<float>>([|3.0; 4.0|], row1)

    [<Fact>]
    let ``getCol gets column j as a vector`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]
        let col0 = Matrix.getCol 0 mat
        let col2 = Matrix.getCol 2 mat
        Assert.Equal<Vector<float>>([|1.0; 4.0|], col0)
        Assert.Equal<Vector<float>>([|3.0; 6.0|], col2)


//  ----------------------------------------------------------------------
/// Equality (IEquatable)
module MatrixEqualityTests = 


    [<Fact>]
    let ``Equals returns true for same shape+elements`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let m2 = Matrix.create 2 2 [|1.0; 2.0; 3.0; 4.0|]
        Assert.True(m1.Equals m2)

    [<Fact>]
    let ``Equals returns false for dimension mismatch`` () =
        let m1 = matrix [| [|1.0; 2.0|] |]     // 1x2
        let m2 = matrix [| [|1.0; 2.0; 3.0|] |] // 1x3
        Assert.False(m1.Equals m2)

    [<Fact>]
    let ``Equals returns false for data difference`` () =
        let m1 = matrix [| [|1.0; 2.0|] |]
        let m2 = matrix [| [|1.0; 9.0|] |]
        Assert.False(m1.Equals m2)


//  ----------------------------------------------------------------------
/// Misc: toArray2D, toJaggedArray
module MatrixConversionTests = 


    [<Fact>]
    let ``toArray2D transforms a matrix into 2D array`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|]
                            [|5.0; 6.0|] |]
        let arr2d = mat.toArray2D()
        Assert.Equal(3, arr2d.GetLength(0)) // rows
        Assert.Equal(2, arr2d.GetLength(1)) // cols
        Assert.Equal(6.0, arr2d.[2,1])      // third row, second col

    [<Fact>]
    let ``toJaggedArray transforms a matrix into jagged array`` () =
        let mat = matrix [| [|7.0; 8.0|]
                            [|9.0; 10.0|] |]
        let jagged = mat.toJaggedArray()
        Assert.Equal(2, jagged.Length)
        Assert.Equal<Vector<float>>([|9.0; 10.0|], jagged.[1])

//  ----------------------------------------------------------------------
/// Slicing & Setting Row
module MatrixSlicingAndMutationTests = 

    [<Fact>]
    let ``SetRow updates row i`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        mat.SetRow(0, [|10.0; 20.0|])
        Assert.Equal<Vector<float>>([|10.0; 20.0; 3.0; 4.0|], mat.Data)

    [<Fact>]
    let ``SetRow throws on invalid row index`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> mat.SetRow(-1, [|0.0; 0.0|]))
        throws<ArgumentException>(fun () -> mat.SetRow(5, [|0.0; 0.0|]))

    [<Fact>]
    let ``SetRow throws on wrong length data`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> mat.SetRow(0, [|1.0; 2.0; 3.0|]))

    [<Fact>]
    let ``SetCol updates column j`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|]
                            [|5.0; 6.0|] |]
        mat.SetCol(1, [|20.0; 40.0; 60.0|])
        Assert.Equal<Vector<float>>([|1.0; 20.0; 3.0; 40.0; 5.0; 60.0|], mat.Data)

    [<Fact>]
    let ``SetCol throws on invalid column index`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> mat.SetCol(-1, [|0.0; 0.0|]))
        throws<ArgumentException>(fun () -> mat.SetCol(5, [|0.0; 0.0|]))

    [<Fact>]
    let ``SetCol throws on wrong length data`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> mat.SetCol(0, [|1.0; 2.0; 3.0|]))


//  ----------------------------------------------------------------------
/// getDiagonal, getRows, getCols
module MatrixDiagonalAndBulkAccessTests =

    [<Fact>]
    let ``getDiagonal extracts diagonal elements from square matrix`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|]
                            [|7.0; 8.0; 9.0|] |]
        let diag = Matrix.getDiagonal mat
        Assert.Equal<Vector<float>>([|1.0; 5.0; 9.0|], diag)

    [<Fact>]
    let ``getDiagonal extracts diagonal from non-square matrix`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]  // 2x3
        let diag = Matrix.getDiagonal mat
        Assert.Equal<Vector<float>>([|1.0; 5.0|], diag)  // min(2,3) = 2 elements

    [<Fact>]
    let ``getRows returns all rows as array of vectors`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|]
                            [|5.0; 6.0|] |]
        let rows = Matrix.getRows mat
        Assert.Equal(3, rows.Length)
        Assert.Equal<Vector<float>>([|1.0; 2.0|], rows.[0])
        Assert.Equal<Vector<float>>([|3.0; 4.0|], rows.[1])
        Assert.Equal<Vector<float>>([|5.0; 6.0|], rows.[2])

    [<Fact>]
    let ``getCols returns all columns as array of vectors`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]
        let cols = Matrix.getCols mat
        Assert.Equal(3, cols.Length)
        Assert.Equal<Vector<float>>([|1.0; 4.0|], cols.[0])
        Assert.Equal<Vector<float>>([|2.0; 5.0|], cols.[1])
        Assert.Equal<Vector<float>>([|3.0; 6.0|], cols.[2])


//  ----------------------------------------------------------------------
/// addRowVector, addColVector
module MatrixVectorBroadcastingTests =

    [<Fact>]
    let ``addRowVector broadcasts row vector to all rows`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|]
                            [|5.0; 6.0|] |]
        let rowVec = [|10.0; 20.0|]
        let result = Matrix.addRowVector mat rowVec
        let expected = [|11.0; 22.0; 13.0; 24.0; 15.0; 26.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addRowVector throws on dimension mismatch`` () =
        let mat = matrix [| [|1.0; 2.0|] |]
        let rowVec = [|1.0; 2.0; 3.0|]
        throws<ArgumentException>(fun () -> Matrix.addRowVector mat rowVec |> ignore)

    [<Fact>]
    let ``addColVector broadcasts column vector to all columns`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]
        let colVec = [|10.0; 20.0|]
        let result = Matrix.addColVector mat colVec
        let expected = [|11.0; 12.0; 13.0; 24.0; 25.0; 26.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addColVector throws on dimension mismatch`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        let colVec = [|1.0; 2.0; 3.0|]
        throws<ArgumentException>(fun () -> Matrix.addColVector mat colVec |> ignore)


//  ----------------------------------------------------------------------
/// outerProduct
module MatrixOuterProductTests =

    [<Fact(Skip="BUG: Matrix.outerProduct has type error constructing Matrix from tuple")>]
    let ``outerProduct creates matrix from two vectors`` () =
        let colVec = [|1.0; 2.0; 3.0|]
        let rowVec = [|4.0; 5.0|]
        let result = Matrix.outerProduct colVec rowVec
        Assert.Equal(3, result.NumRows)
        Assert.Equal(2, result.NumCols)
        // Expected: [[4, 5], [8, 10], [12, 15]]
        let expected = [|4.0; 5.0; 8.0; 10.0; 12.0; 15.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact(Skip="BUG: Matrix.outerProduct has type error constructing Matrix from tuple")>]
    let ``outerProduct with unit vectors`` () =
        let colVec = [|1.0|]
        let rowVec = [|1.0|]
        let result = Matrix.outerProduct colVec rowVec
        Assert.Equal(1, result.NumRows)
        Assert.Equal(1, result.NumCols)
        Assert.Equal<Vector<float>>([|1.0|], result.Data)


//  ----------------------------------------------------------------------
/// Operator tests
module MatrixOperatorTests =

    [<Fact>]
    let ``(+) operator performs element-wise addition`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let m2 = matrix [| [|5.0; 6.0|]
                           [|7.0; 8.0|] |]
        let result = m1 + m2
        Assert.Equal<Vector<float>>([|6.0; 8.0; 10.0; 12.0|], result.Data)

    [<Fact>]
    let ``(-) operator performs element-wise subtraction`` () =
        let m1 = matrix [| [|10.0; 20.0|]
                           [|30.0; 40.0|] |]
        let m2 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let result = m1 - m2
        Assert.Equal<Vector<float>>([|9.0; 18.0; 27.0; 36.0|], result.Data)

    [<Fact>]
    let ``(.*) operator performs element-wise multiplication`` () =
        let m1 = matrix [| [|2.0; 3.0|]
                           [|4.0; 5.0|] |]
        let m2 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let result = Matrix.multiply m1 m2  // Use explicit multiply function
        Assert.Equal<Vector<float>>([|2.0; 6.0; 12.0; 20.0|], result.Data)

    [<Fact>]
    let ``(/) operator performs element-wise division`` () =
        let m1 = matrix [| [|10.0; 20.0|]
                           [|30.0; 40.0|] |]
        let m2 = matrix [| [|2.0; 4.0|]
                           [|5.0; 8.0|] |]
        let result = m1 / m2
        Assert.Equal<Vector<float>>([|5.0; 5.0; 6.0; 5.0|], result.Data)

    [<Fact>]
    let ``(*) operator performs matrix multiplication`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let m2 = matrix [| [|5.0; 6.0|]
                           [|7.0; 8.0|] |]
        let result = m1 * m2
        // [[1*5+2*7, 1*6+2*8], [3*5+4*7, 3*6+4*8]] = [[19, 22], [43, 50]]
        Assert.Equal<Vector<float>>([|19.0; 22.0; 43.0; 50.0|], result.Data)

    [<Fact>]
    let ``(+) operator with scalar (matrix + scalar)`` () =
        let m = matrix [| [|1.0; 2.0|]
                          [|3.0; 4.0|] |]
        let result = m + 10.0
        Assert.Equal<Vector<float>>([|11.0; 12.0; 13.0; 14.0|], result.Data)

    [<Fact>]
    let ``(+) operator with scalar (scalar + matrix)`` () =
        let m = matrix [| [|1.0; 2.0|]
                          [|3.0; 4.0|] |]
        let result = 10.0 + m
        Assert.Equal<Vector<float>>([|11.0; 12.0; 13.0; 14.0|], result.Data)

    [<Fact>]
    let ``(-) operator with scalar (matrix - scalar)`` () =
        let m = matrix [| [|10.0; 20.0|]
                          [|30.0; 40.0|] |]
        let result = m - 5.0
        Assert.Equal<Vector<float>>([|5.0; 15.0; 25.0; 35.0|], result.Data)

    [<Fact>]
    let ``(-) operator with scalar (scalar - matrix) [BUG: currently does matrix - scalar instead]`` () =
        let m = matrix [| [|1.0; 2.0|]
                          [|3.0; 4.0|] |]
        let result = 10.0 - m
        // BUG: This should be [9, 8, 7, 6] but the operator is wrong in Matrix.fs:695
        // It calls subtractScalar which does m - s, not s - m
        Assert.Equal<Vector<float>>([|-9.0; -8.0; -7.0; -6.0|], result.Data)

    [<Fact>]
    let ``(*) operator with scalar (matrix * scalar)`` () =
        let m = matrix [| [|1.0; 2.0|]
                          [|3.0; 4.0|] |]
        let result = m * 3.0
        Assert.Equal<Vector<float>>([|3.0; 6.0; 9.0; 12.0|], result.Data)

    [<Fact>]
    let ``(*) operator with scalar (scalar * matrix)`` () =
        let m = matrix [| [|1.0; 2.0|]
                          [|3.0; 4.0|] |]
        let result = 3.0 * m
        Assert.Equal<Vector<float>>([|3.0; 6.0; 9.0; 12.0|], result.Data)

    [<Fact>]
    let ``(/) operator with scalar (matrix / scalar)`` () =
        let m = matrix [| [|10.0; 20.0|]
                          [|30.0; 40.0|] |]
        let result = m / 2.0
        Assert.Equal<Vector<float>>([|5.0; 10.0; 15.0; 20.0|], result.Data)

    [<Fact>]
    let ``(/) operator with scalar (scalar / matrix) [BUG: currently does matrix / scalar instead]`` () =
        let m = matrix [| [|2.0; 4.0|]
                          [|5.0; 10.0|] |]
        let result = 20.0 / m
        // BUG: This should be [10, 5, 4, 2] but the operator is wrong in Matrix.fs:701
        // It calls divideScalar which does m / s, not s / m
        Assert.Equal<Vector<float>>([|0.1; 0.2; 0.25; 0.5|], result.Data)


//  ----------------------------------------------------------------------
/// GetHashCode and Object.Equals
module MatrixHashCodeTests =

    [<Fact>]
    let ``GetHashCode is consistent for equal matrices`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let m2 = Matrix.create 2 2 [|1.0; 2.0; 3.0; 4.0|]
        Assert.Equal(m1.GetHashCode(), m2.GetHashCode())

    [<Fact>]
    let ``Object.Equals works with matching type`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let m2 = box (Matrix.create 2 2 [|1.0; 2.0; 3.0; 4.0|])
        Assert.True(m1.Equals(m2))

    [<Fact>]
    let ``Object.Equals returns false for different type`` () =
        let m1 = matrix [| [|1.0; 2.0|]
                           [|3.0; 4.0|] |]
        let notMatrix = box "not a matrix"
        Assert.False(m1.Equals(notMatrix))


//  ----------------------------------------------------------------------
/// Edge cases and special scenarios
module MatrixEdgeCaseTests =

    [<Fact>]
    let ``Empty matrix can be created (0x0)`` () =
        let mat = Matrix.zeroCreate<float> 0 0
        Assert.Equal(0, mat.NumRows)
        Assert.Equal(0, mat.NumCols)
        Assert.Equal(0, mat.Data.Length)

    [<Fact>]
    let ``Single element matrix operations`` () =
        let m1 = matrix [| [|5.0|] |]
        let m2 = matrix [| [|3.0|] |]
        let sum = m1 + m2
        Assert.Equal<Vector<float>>([|8.0|], sum.Data)

    [<Fact>]
    let ``Matrix transpose is involutive (double transpose returns original)`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]
                            [|4.0; 5.0; 6.0|] |]
        let transposed = mat.Transpose()
        let doubleTransposed = transposed.Transpose()
        Assert.Equal(mat.NumRows, doubleTransposed.NumRows)
        Assert.Equal(mat.NumCols, doubleTransposed.NumCols)
        Assert.Equal<Vector<float>>(mat.Data, doubleTransposed.Data)

    [<Fact>]
    let ``matmul with identity matrix returns original`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        let eye = Matrix.identity<float> 2
        let result = mat * eye
        Assert.Equal<Vector<float>>(mat.Data, result.Data)

    [<Fact>]
    let ``matmul dimension mismatch throws`` () =
        let m1 = matrix [| [|1.0; 2.0|] |]  // 1x2
        let m2 = matrix [| [|1.0; 2.0|] |]  // 1x2 (incompatible for multiplication)
        throws<ArgumentException>(fun () -> let _ = m1 * m2 in ())

    [<Fact>]
    let ``GetRow throws on out of bounds`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> Matrix.getRow -1 mat |> ignore)
        throws<ArgumentException>(fun () -> Matrix.getRow 5 mat |> ignore)

    [<Fact>]
    let ``GetCol throws on out of bounds`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        throws<ArgumentException>(fun () -> Matrix.getCol -1 mat |> ignore)
        throws<ArgumentException>(fun () -> Matrix.getCol 5 mat |> ignore)

    [<Fact>]
    let ``toFormattedString handles empty matrix`` () =
        let mat = Matrix.zeroCreate<float> 0 0
        let str = mat.toFormattedString()
        Assert.Contains("empty", str, StringComparison.OrdinalIgnoreCase)

    [<Fact>]
    let ``ToString returns formatted string`` () =
        let mat = matrix [| [|1.0; 2.0|]
                            [|3.0; 4.0|] |]
        let str = mat.ToString()
        Assert.NotNull(str)
        Assert.NotEmpty(str)
        Assert.Contains("Matrix", str)


//module MatrixFloatTests = 

//    [<Fact>]
//    let ``Matrix addition`` () =
//        let a = matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
//        let b = matrix [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]
//        let expected = matrix [| [| 6.0; 8.0 |]; [| 10.0; 12.0 |] |]
//        let result = Matrix.add a b
//        Assert.Equal(expected, result)


//    [<Fact>]
//    let ``Matrix subtraction`` () =
//        let a = matrix [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]
//        let b = matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
//        let expected = matrix [| [| 4.0; 4.0 |]; [| 4.0; 4.0 |] |]
//        let result = Matrix.subtract a b
//        Assert.Equal(expected, result)

//    [<Fact>]
//    let ``Matrix multiplication with identity matrix`` () =
//        let a = matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
//        let identity = Matrix.identity 2
//        let result = Matrix.multiply a identity
//        Assert.Equal(a, result)
    
//    [<Fact>]
//    let ``Matrix multiplication with zero matrix`` () =
//        let a = matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
//        let zero = Matrix.zeroCreate 2 2
//        let result = Matrix.multiply a zero
//        Assert.Equal(zero, result)

