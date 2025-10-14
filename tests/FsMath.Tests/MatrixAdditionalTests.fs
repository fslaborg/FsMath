namespace FsMath.Tests.Matrix

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers

/// Additional comprehensive tests for Matrix to improve coverage
module MatrixSliceAdditionalTests =

    [<Fact>]
    let ``GetSlice with full row range`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
        |]
        let slice = mat.[*, 1..2]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|2.0; 3.0; 6.0; 7.0; 10.0; 11.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with full column range`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
            [|10.0; 11.0; 12.0|]
        |]
        let slice = mat.[1..2, *]
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|4.0; 5.0; 6.0; 7.0; 8.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with single row`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        let slice = mat.[1..1, 0..2]
        Assert.Equal(1, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|4.0; 5.0; 6.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with single column`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        let slice = mat.[0..2, 1..1]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(1, slice.NumCols)
        Assert.Equal<Vector<float>>([|2.0; 5.0; 8.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with entire matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        let slice = mat.[*, *]
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0|], slice.Data)

    [<Fact>]
    let ``GetSlice throws on out of range rows`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        throws<ArgumentException>(fun () -> mat.[0..5, *] |> ignore)

    [<Fact>]
    let ``GetSlice throws on out of range columns`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        throws<ArgumentException>(fun () -> mat.[*, 0..5] |> ignore)


module MatrixTransposeAdditionalTests =

    [<Fact>]
    let ``transpose of rectangular matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let result = Matrix.transpose mat
        Assert.Equal(3, result.NumRows)
        Assert.Equal(2, result.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 4.0; 2.0; 5.0; 3.0; 6.0|], result.Data)

    [<Fact>]
    let ``transpose of square matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        let result = Matrix.transpose mat
        Assert.Equal(3, result.NumRows)
        Assert.Equal(3, result.NumCols)
        // Rows become columns
        Assert.Equal<Vector<float>>([|1.0; 4.0; 7.0; 2.0; 5.0; 8.0; 3.0; 6.0; 9.0|], result.Data)

    [<Fact>]
    let ``transpose of single element matrix`` () =
        let mat = matrix [| [|42.0|] |]
        let result = Matrix.transpose mat
        Assert.Equal(1, result.NumRows)
        Assert.Equal(1, result.NumCols)
        Assert.Equal<Vector<float>>([|42.0|], result.Data)

    [<Fact>]
    let ``transpose of column vector`` () =
        let mat = matrix [|
            [|1.0|]
            [|2.0|]
            [|3.0|]
        |]
        let result = Matrix.transpose mat
        Assert.Equal(1, result.NumRows)
        Assert.Equal(3, result.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0|], result.Data)

    [<Fact>]
    let ``transpose of row vector`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0; 4.0|] |]
        let result = Matrix.transpose mat
        Assert.Equal(4, result.NumRows)
        Assert.Equal(1, result.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0|], result.Data)

    [<Fact>]
    let ``transpose is involutive`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let transposed = Matrix.transpose mat
        let doubleTransposed = Matrix.transpose transposed
        Assert.Equal(mat.NumRows, doubleTransposed.NumRows)
        Assert.Equal(mat.NumCols, doubleTransposed.NumCols)
        Assert.Equal<Vector<float>>(mat.Data, doubleTransposed.Data)


module MatrixArithmeticAdditionalTests =

    [<Fact>]
    let ``add with same dimensions`` () =
        let m1 = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let m2 = matrix [|
            [|10.0; 20.0; 30.0|]
            [|40.0; 50.0; 60.0|]
        |]
        let result = Matrix.add m1 m2
        Assert.Equal<Vector<float>>([|11.0; 22.0; 33.0; 44.0; 55.0; 66.0|], result.Data)

    [<Fact>]
    let ``subtract with same dimensions`` () =
        let m1 = matrix [|
            [|100.0; 200.0|]
            [|300.0; 400.0|]
        |]
        let m2 = matrix [|
            [|10.0; 20.0|]
            [|30.0; 40.0|]
        |]
        let result = Matrix.subtract m1 m2
        Assert.Equal<Vector<float>>([|90.0; 180.0; 270.0; 360.0|], result.Data)

    [<Fact>]
    let ``multiply element-wise`` () =
        let m1 = matrix [|
            [|2.0; 3.0|]
            [|4.0; 5.0|]
        |]
        let m2 = matrix [|
            [|10.0; 10.0|]
            [|10.0; 10.0|]
        |]
        let result = Matrix.multiply m1 m2
        Assert.Equal<Vector<float>>([|20.0; 30.0; 40.0; 50.0|], result.Data)

    [<Fact>]
    let ``divide element-wise`` () =
        let m1 = matrix [|
            [|100.0; 200.0|]
            [|300.0; 400.0|]
        |]
        let m2 = matrix [|
            [|10.0; 20.0|]
            [|30.0; 40.0|]
        |]
        let result = Matrix.divide m1 m2
        Assert.Equal<Vector<float>>([|10.0; 10.0; 10.0; 10.0|], result.Data)

    [<Fact>]
    let ``add throws on dimension mismatch`` () =
        let m1 = matrix [| [|1.0; 2.0|] |]
        let m2 = matrix [| [|1.0; 2.0; 3.0|] |]
        throws<ArgumentException>(fun () -> Matrix.add m1 m2 |> ignore)

    [<Fact>]
    let ``multiply throws on dimension mismatch`` () =
        let m1 = matrix [| [|1.0; 2.0|] |]
        let m2 = matrix [| [|1.0; 2.0; 3.0|] |]
        throws<ArgumentException>(fun () -> Matrix.multiply m1 m2 |> ignore)


module MatrixScalarOperationsAdditionalTests =

    [<Fact>]
    let ``addScalar adds scalar to all elements`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let result = Matrix.addScalar mat 100.0
        Assert.Equal<Vector<float>>([|101.0; 102.0; 103.0; 104.0; 105.0; 106.0|], result.Data)

    [<Fact>]
    let ``subtractScalar subtracts scalar from all elements`` () =
        let mat = matrix [|
            [|10.0; 20.0|]
            [|30.0; 40.0|]
        |]
        let result = Matrix.subtractScalar mat 5.0
        Assert.Equal<Vector<float>>([|5.0; 15.0; 25.0; 35.0|], result.Data)

    [<Fact>]
    let ``multiplyScalar multiplies all elements by scalar`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        let result = Matrix.multiplyScalar mat 10.0
        Assert.Equal<Vector<float>>([|10.0; 20.0; 30.0; 40.0|], result.Data)

    [<Fact>]
    let ``divideScalar divides all elements by scalar`` () =
        let mat = matrix [|
            [|100.0; 200.0|]
            [|300.0; 400.0|]
        |]
        let result = Matrix.divideScalar mat 100.0
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0|], result.Data)

    [<Fact>]
    let ``addScalar with zero`` () =
        let mat = matrix [| [|1.0; 2.0|] |]
        let result = Matrix.addScalar mat 0.0
        Assert.Equal<Vector<float>>(mat.Data, result.Data)

    [<Fact>]
    let ``multiplyScalar with one`` () =
        let mat = matrix [| [|1.0; 2.0|] |]
        let result = Matrix.multiplyScalar mat 1.0
        Assert.Equal<Vector<float>>(mat.Data, result.Data)


module MatrixVectorOperationsAdditionalTests =

    [<Fact>]
    let ``multiplyRowVector performs vector-matrix multiplication`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        let rowVec = [|10.0; 100.0|]
        let result = Matrix.multiplyRowVector rowVec mat
        // Row vector [10, 100] * [[1,2],[3,4]] = [10*1+100*3, 10*2+100*4] = [310, 420]
        Assert.Equal<Vector<float>>([|310.0; 420.0|], result)

    [<Fact>]
    let ``multiplyRowVector throws on dimension mismatch`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|] |]  // 1x3 matrix
        let rowVec = [|1.0; 2.0|]  // length 2
        throws<ArgumentException>(fun () -> Matrix.multiplyRowVector rowVec mat |> ignore)

    [<Fact>]
    let ``addRowVector with zeros`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        let rowVec = [|0.0; 0.0|]
        let result = Matrix.addRowVector mat rowVec
        Assert.Equal<Vector<float>>(mat.Data, result.Data)

    [<Fact>]
    let ``addColVector with different matrix shapes`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
        |]
        let colVec = [|100.0; 200.0; 300.0|]
        let result = Matrix.addColVector mat colVec
        let expected = [|101.0; 102.0; 103.0; 104.0; 205.0; 206.0; 207.0; 208.0; 309.0; 310.0; 311.0; 312.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addColVector with single column matrix`` () =
        let mat = matrix [|
            [|1.0|]
            [|2.0|]
            [|3.0|]
        |]
        let colVec = [|10.0; 20.0; 30.0|]
        let result = Matrix.addColVector mat colVec
        Assert.Equal<Vector<float>>([|11.0; 22.0; 33.0|], result.Data)


module MatrixVectorMultiplicationAdditionalTests =

    [<Fact>]
    let ``muliplyVector with column vector`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let vec = [|10.0; 20.0; 30.0|]
        let result = Matrix.muliplyVector mat vec
        // [1*10 + 2*20 + 3*30, 4*10 + 5*20 + 6*30]
        // [10 + 40 + 90, 40 + 100 + 180]
        // [140, 320]
        Assert.Equal<Vector<float>>([|140.0; 320.0|], result)

    [<Fact>]
    let ``muliplyVector with identity matrix`` () =
        let mat = Matrix.identity<float> 3
        let vec = [|5.0; 10.0; 15.0|]
        let result = Matrix.muliplyVector mat vec
        Assert.Equal<Vector<float>>(vec, result)

    [<Fact>]
    let ``muliplyVector with zero vector`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
        |]
        let vec = [|0.0; 0.0|]
        let result = Matrix.muliplyVector mat vec
        Assert.Equal<Vector<float>>([|0.0; 0.0|], result)

    [<Fact>]
    let ``muliplyVector throws on dimension mismatch`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let vec = [|10.0; 20.0|]  // Wrong size
        throws<ArgumentException>(fun () -> Matrix.muliplyVector mat vec |> ignore)

    [<Fact>]
    let ``muliplyVector with single element`` () =
        let mat = matrix [| [|5.0|] |]
        let vec = [|10.0|]
        let result = Matrix.muliplyVector mat vec
        Assert.Equal<Vector<float>>([|50.0|], result)

    [<Fact>]
    let ``muliplyVector with tall matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
            [|5.0; 6.0|]
            [|7.0; 8.0|]
        |]
        let vec = [|10.0; 100.0|]
        let result = Matrix.muliplyVector mat vec
        Assert.Equal<Vector<float>>([|210.0; 430.0; 650.0; 870.0|], result)


module MatrixOperatorAdditionalTests =

    [<Fact>]
    let ``operator (+) with matrices`` () =
        let m1 = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let m2 = matrix [| [|5.0; 6.0|]; [|7.0; 8.0|] |]
        let result = m1 + m2
        Assert.Equal<Vector<float>>([|6.0; 8.0; 10.0; 12.0|], result.Data)

    [<Fact>]
    let ``operator (-) with matrices`` () =
        let m1 = matrix [| [|10.0; 20.0|]; [|30.0; 40.0|] |]
        let m2 = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let result = m1 - m2
        Assert.Equal<Vector<float>>([|9.0; 18.0; 27.0; 36.0|], result.Data)

    [<Fact>]
    let ``operator (*) with matrices performs matmul`` () =
        let m1 = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let m2 = matrix [| [|5.0; 6.0|]; [|7.0; 8.0|] |]
        let result = m1 * m2
        Assert.Equal<Vector<float>>([|19.0; 22.0; 43.0; 50.0|], result.Data)

    [<Fact>]
    let ``operator (/) with matrices performs element-wise division`` () =
        let m1 = matrix [| [|100.0; 200.0|]; [|300.0; 400.0|] |]
        let m2 = matrix [| [|10.0; 20.0|]; [|30.0; 40.0|] |]
        let result = m1 / m2
        Assert.Equal<Vector<float>>([|10.0; 10.0; 10.0; 10.0|], result.Data)

    [<Fact>]
    let ``operator (+) matrix with scalar`` () =
        let mat = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let result = mat + 10.0
        Assert.Equal<Vector<float>>([|11.0; 12.0; 13.0; 14.0|], result.Data)

    [<Fact>]
    let ``operator (+) scalar with matrix`` () =
        let mat = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let result = 10.0 + mat
        Assert.Equal<Vector<float>>([|11.0; 12.0; 13.0; 14.0|], result.Data)

    [<Fact>]
    let ``operator (-) matrix with scalar`` () =
        let mat = matrix [| [|10.0; 20.0|]; [|30.0; 40.0|] |]
        let result = mat - 5.0
        Assert.Equal<Vector<float>>([|5.0; 15.0; 25.0; 35.0|], result.Data)

    [<Fact>]
    let ``operator (*) matrix with scalar`` () =
        let mat = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let result = mat * 10.0
        Assert.Equal<Vector<float>>([|10.0; 20.0; 30.0; 40.0|], result.Data)

    [<Fact>]
    let ``operator (*) scalar with matrix`` () =
        let mat = matrix [| [|1.0; 2.0|]; [|3.0; 4.0|] |]
        let result = 10.0 * mat
        Assert.Equal<Vector<float>>([|10.0; 20.0; 30.0; 40.0|], result.Data)

    [<Fact>]
    let ``operator (/) matrix with scalar`` () =
        let mat = matrix [| [|100.0; 200.0|]; [|300.0; 400.0|] |]
        let result = mat / 100.0
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0|], result.Data)

    [<Fact>]
    let ``operator (*) matrix with vector`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|] |]
        let vec = [|10.0; 20.0; 30.0|]
        let result = mat * vec
        Assert.Equal<Vector<float>>([|140.0; 320.0|], result)

    [<Fact>]
    let ``operator (*) vector with matrix`` () =
        let vec = [|2.0; 3.0|]
        let mat = matrix [| [|10.0; 100.0|]; [|20.0; 200.0|] |]
        let result = vec * mat
        Assert.Equal<Vector<float>>([|80.0; 800.0|], result)


module MatrixEdgeCasesAdditionalTests =

    [<Fact>]
    let ``operations on 1x1 matrices`` () =
        let m1 = matrix [| [|5.0|] |]
        let m2 = matrix [| [|3.0|] |]
        Assert.Equal<Vector<float>>([|8.0|], (m1 + m2).Data)
        Assert.Equal<Vector<float>>([|2.0|], (m1 - m2).Data)
        Assert.Equal<Vector<float>>([|15.0|], (Matrix.multiply m1 m2).Data)

    [<Fact>]
    let ``transpose of large matrix`` () =
        let mat = Matrix.init 10 15 (fun i j -> float (i * 15 + j))
        let transposed = Matrix.transpose mat
        Assert.Equal(15, transposed.NumRows)
        Assert.Equal(10, transposed.NumCols)
        Assert.Equal(mat.[0, 0], transposed.[0, 0])
        Assert.Equal(mat.[9, 14], transposed.[14, 9])

    [<Fact>]
    let ``slice of slice`` () =
        let mat = Matrix.init 5 5 (fun i j -> float (i * 5 + j))
        let slice1 = mat.[1..3, 1..3]
        let slice2 = slice1.[0..1, 0..1]
        Assert.Equal(2, slice2.NumRows)
        Assert.Equal(2, slice2.NumCols)

    [<Fact>]
    let ``scalar operations preserve dimensions`` () =
        let mat = matrix [| [|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|] |]
        let result = Matrix.addScalar mat 10.0
        Assert.Equal(mat.NumRows, result.NumRows)
        Assert.Equal(mat.NumCols, result.NumCols)

    [<Fact>]
    let ``element-wise operations preserve dimensions`` () =
        let m1 = matrix [| [|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|] |]
        let m2 = matrix [| [|10.0; 20.0; 30.0|]; [|40.0; 50.0; 60.0|] |]
        let result = Matrix.add m1 m2
        Assert.Equal(m1.NumRows, result.NumRows)
        Assert.Equal(m1.NumCols, result.NumCols)
