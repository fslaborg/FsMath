namespace FsMath.Tests.Matrix

open System
open Xunit
open FsMath
open FsMath.Tests
open FsMath.Tests.AssertHelpers

//  ----------------------------------------------------------------------
/// Tests for additional Matrix slicing edge cases
module MatrixGetSliceEdgeCaseTests =

    [<Fact>]
    let ``GetSlice with optional None parameters - full matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Call GetSlice with all None to get full matrix
        let slice = mat.GetSlice(None, None, None, None)
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with Some and None mixed`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Rows 1 to end, cols 1 to end
        let slice = mat.GetSlice(Some 1, None, Some 1, None)
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|5.0; 6.0; 8.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with explicit Some values`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
        |]
        // Extract middle 2x2
        let slice = mat.GetSlice(Some 1, Some 2, Some 1, Some 2)
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|6.0; 7.0; 10.0; 11.0|], slice.Data)

    [<Fact>]
    let ``GetSlice with None for rows, Some for cols`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // All rows, cols 1 to 2
        let slice = mat.GetSlice(None, None, Some 1, Some 2)
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|2.0; 3.0; 5.0; 6.0; 8.0; 9.0|], slice.Data)


//  ----------------------------------------------------------------------
/// Tests for Matrix.transposeByBlock (currently 0% coverage, private inline)
/// We test via the public Transpose() method which calls transposeByBlock
module MatrixTransposeBlockTests =

    [<Fact>]
    let ``Transpose with various block sizes - 3x3`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        let t = mat.Transpose()
        Assert.Equal(3, t.NumRows)
        Assert.Equal(3, t.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 4.0; 7.0; 2.0; 5.0; 8.0; 3.0; 6.0; 9.0|], t.Data)

    [<Fact>]
    let ``Transpose with block optimization - 4x4 matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
            [|13.0; 14.0; 15.0; 16.0|]
        |]
        let t = mat.Transpose()
        Assert.Equal(4, t.NumRows)
        Assert.Equal(4, t.NumCols)
        let expected = [|1.0; 5.0; 9.0; 13.0; 2.0; 6.0; 10.0; 14.0;
                        3.0; 7.0; 11.0; 15.0; 4.0; 8.0; 12.0; 16.0|]
        Assert.Equal<Vector<float>>(expected, t.Data)

    [<Fact>]
    let ``Transpose with block optimization - large square matrix 16x16`` () =
        // Create 16x16 matrix to test block traversal
        let mat = Matrix.init<float> 16 16 (fun i j -> float (i * 16 + j))
        let t = mat.Transpose()
        Assert.Equal(16, t.NumRows)
        Assert.Equal(16, t.NumCols)
        // Verify a few key elements
        Assert.Equal(0.0, t.[0, 0])   // (0,0) -> (0,0)
        Assert.Equal(16.0, t.[0, 1])  // (1,0) -> (0,1)
        Assert.Equal(1.0, t.[1, 0])   // (0,1) -> (1,0)
        Assert.Equal(255.0, t.[15, 15]) // (15,15) -> (15,15)

    [<Fact>]
    let ``Transpose with block optimization - tall matrix 8x3`` () =
        let mat = Matrix.init<float> 8 3 (fun i j -> float (i * 3 + j))
        let t = mat.Transpose()
        Assert.Equal(3, t.NumRows)
        Assert.Equal(8, t.NumCols)
        // Verify transpose correctness
        for i in 0..7 do
            for j in 0..2 do
                Assert.Equal(mat.[i, j], t.[j, i])

    [<Fact>]
    let ``Transpose with block optimization - wide matrix 3x8`` () =
        let mat = Matrix.init<float> 3 8 (fun i j -> float (i * 8 + j))
        let t = mat.Transpose()
        Assert.Equal(8, t.NumRows)
        Assert.Equal(3, t.NumCols)
        // Verify transpose correctness
        for i in 0..2 do
            for j in 0..7 do
                Assert.Equal(mat.[i, j], t.[j, i])

    [<Fact>]
    let ``Transpose with block optimization - very large matrix 32x32`` () =
        // Test with matrix larger than typical block size (16)
        let mat = Matrix.init<float> 32 32 (fun i j -> float (i + j))
        let t = mat.Transpose()
        Assert.Equal(32, t.NumRows)
        Assert.Equal(32, t.NumCols)
        // Verify symmetry property for this specific matrix (i+j = j+i)
        for i in 0..31 do
            for j in 0..31 do
                Assert.Equal(mat.[i, j], t.[j, i])

    [<Fact>]
    let ``Transpose with block optimization - single row`` () =
        let mat = matrix [|[|1.0; 2.0; 3.0; 4.0; 5.0|]|]
        let t = mat.Transpose()
        Assert.Equal(5, t.NumRows)
        Assert.Equal(1, t.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0; 5.0|], t.Data)

    [<Fact>]
    let ``Transpose with block optimization - single column`` () =
        let mat = matrix [|
            [|1.0|]
            [|2.0|]
            [|3.0|]
            [|4.0|]
            [|5.0|]
        |]
        let t = mat.Transpose()
        Assert.Equal(1, t.NumRows)
        Assert.Equal(5, t.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0; 5.0|], t.Data)


//  ----------------------------------------------------------------------
/// Tests for Matrix.multiplyRowVector edge cases (currently 43.3% coverage)
/// Need to test non-SIMD fallback path and edge cases
module MatrixMultiplyRowVectorTests =

    [<Fact>]
    let ``multiplyRowVector - small matrix non-SIMD path`` () =
        // Use 1x1 matrix to test non-SIMD fallback
        let v = [|2.0|]
        let mat = matrix [|[|5.0|]|]
        let result = v * mat
        Assert.Equal<Vector<float>>([|10.0|], result)

    [<Fact>]
    let ``multiplyRowVector - 2x2 matrix`` () =
        let v = [|2.0; 3.0|]
        let mat = matrix [|
            [|1.0; 4.0|]
            [|2.0; 5.0|]
        |]
        // v × M = [2*1 + 3*2, 2*4 + 3*5] = [8, 23]
        let result = v * mat
        Assert.Equal<Vector<float>>([|8.0; 23.0|], result)

    [<Fact>]
    let ``multiplyRowVector - with zero weights`` () =
        // Test the optimization path where weight = 0
        let v = [|0.0; 1.0; 0.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Only middle row should contribute
        let result = v * mat
        Assert.Equal<Vector<float>>([|4.0; 5.0; 6.0|], result)

    [<Fact>]
    let ``multiplyRowVector - all zero weights`` () =
        let v = [|0.0; 0.0; 0.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        let result = v * mat
        Assert.Equal<Vector<float>>([|0.0; 0.0; 0.0|], result)

    [<Fact>]
    let ``multiplyRowVector - small matrix cols less than SIMD width`` () =
        // Create matrix with fewer columns than typical SIMD width (< 4)
        let v = [|1.0; 2.0|]
        let mat = matrix [|
            [|3.0; 4.0|]
            [|5.0; 6.0|]
        |]
        // v × M = [1*3 + 2*5, 1*4 + 2*6] = [13, 16]
        let result = v * mat
        Assert.Equal<Vector<float>>([|13.0; 16.0|], result)

    [<Fact>]
    let ``multiplyRowVector - tall matrix many rows`` () =
        // Test with many rows (10 rows, 3 cols)
        let v = [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0|]
        let mat = Matrix.init<float> 10 3 (fun i j -> float (i + j))
        let result = v * mat
        // Manually compute expected: weighted sum of rows
        let mutable expected = Array.zeroCreate<float> 3
        for i in 0..9 do
            for j in 0..2 do
                expected.[j] <- expected.[j] + v.[i] * mat.[i, j]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector - wide matrix many cols`` () =
        // Test with many cols (3 rows, 10 cols)
        let v = [|1.0; 2.0; 3.0|]
        let mat = Matrix.init<float> 3 10 (fun i j -> float (i * 10 + j))
        let result = v * mat
        // Manually compute expected
        let mutable expected = Array.zeroCreate<float> 10
        for i in 0..2 do
            for j in 0..9 do
                expected.[j] <- expected.[j] + v.[i] * mat.[i, j]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector - negative weights`` () =
        let v = [|-1.0; 2.0; -3.0|]
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
            [|5.0; 6.0|]
        |]
        // v × M = [-1*1 + 2*3 + (-3)*5, -1*2 + 2*4 + (-3)*6] = [-10, -12]
        let result = v * mat
        floatArrayClose [|-10.0; -12.0|] result 1e-10

    [<Fact>]
    let ``multiplyRowVector - dimension mismatch throws`` () =
        let v = [|1.0; 2.0|]
        let mat = matrix [|
            [|1.0; 2.0|]
            [|3.0; 4.0|]
            [|5.0; 6.0|]
        |]
        // v.Length (2) does not match mat.NumRows (3)
        throws<ArgumentException>(fun () -> let _ = v * mat in ())

    [<Fact>]
    let ``multiplyRowVector - single row matrix`` () =
        let v = [|5.0|]
        let mat = matrix [|[|2.0; 3.0; 4.0|]|]
        // v × M = [5*2, 5*3, 5*4] = [10, 15, 20]
        let result = v * mat
        Assert.Equal<Vector<float>>([|10.0; 15.0; 20.0|], result)

    [<Fact>]
    let ``multiplyRowVector - fractional weights`` () =
        let v = [|0.5; 0.3; 0.2|]
        let mat = matrix [|
            [|10.0; 20.0|]
            [|30.0; 40.0|]
            [|50.0; 60.0|]
        |]
        // v × M = [0.5*10 + 0.3*30 + 0.2*50, 0.5*20 + 0.3*40 + 0.2*60]
        //       = [24, 34]
        let result = v * mat
        floatArrayClose [|24.0; 34.0|] result 1e-10


//  ----------------------------------------------------------------------
/// Additional edge case tests for muliplyVector (matrix * vector)
/// To improve line coverage for the uncovered line in that function
module MatrixMultiplyVectorEdgeCaseTests =

    [<Fact>]
    let ``muliplyVector - very small vector length 1`` () =
        let mat = matrix [|
            [|5.0|]
            [|10.0|]
        |]
        let v = [|3.0|]
        let result = mat * v
        Assert.Equal<Vector<float>>([|15.0; 30.0|], result)

    [<Fact>]
    let ``muliplyVector - vector length not multiple of SIMD size`` () =
        // Use length 3 which is not a multiple of typical SIMD width (4)
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let v = [|1.0; 2.0; 3.0|]
        // mat × v = [1*1 + 2*2 + 3*3, 4*1 + 5*2 + 6*3] = [14, 32]
        let result = mat * v
        Assert.Equal<Vector<float>>([|14.0; 32.0|], result)

    [<Fact>]
    let ``muliplyVector - larger matrix with tail elements`` () =
        // Use 5 columns to ensure there are tail elements after SIMD processing
        let mat = Matrix.init<float> 3 5 (fun i j -> float (i * 5 + j + 1))
        let v = [|1.0; 1.0; 1.0; 1.0; 1.0|]
        let result = mat * v
        // Each row sum should be the sum of its elements
        Assert.Equal(15.0, result.[0])  // 1+2+3+4+5 = 15
        Assert.Equal(40.0, result.[1])  // 6+7+8+9+10 = 40
        Assert.Equal(65.0, result.[2])  // 11+12+13+14+15 = 65


//  ----------------------------------------------------------------------
/// Additional edge case tests for addRowVector and addColVector
/// To improve line coverage for uncovered lines in those functions
module MatrixVectorAddEdgeCaseTests =

    [<Fact>]
    let ``addRowVector - small matrix with tail elements`` () =
        // Use 3 columns to test scalar tail processing
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let v = [|10.0; 20.0; 30.0|]
        let result = Matrix.addRowVector mat v
        let expected = [|11.0; 22.0; 33.0; 14.0; 25.0; 36.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addColVector - small matrix with tail elements`` () =
        // Use 3 columns to test scalar tail processing
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
        |]
        let v = [|100.0; 200.0|]
        let result = Matrix.addColVector mat v
        let expected = [|101.0; 102.0; 103.0; 204.0; 205.0; 206.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)


//  ----------------------------------------------------------------------
/// Tests for Matrix.GetSlice(Range, Range) - the F# Range-based slice syntax
/// This tests the convenient mat.[r1..r2, c1..c2] syntax
module MatrixRangeGetSliceTests =

    [<Fact>]
    let ``GetSlice Range - full range with .. operator`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Using 0.. to get full matrix
        let slice = mat.[0.., 0..]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - explicit start and end indices`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
            [|13.0; 14.0; 15.0; 16.0|]
        |]
        // Extract middle 2x2: mat.[1..2, 1..2]
        let slice = mat.[1..2, 1..2]
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|6.0; 7.0; 10.0; 11.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - from start to specific index`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
        |]
        // Get first 2 rows, first 3 cols: mat.[..1, ..2]
        let slice = mat.[..1, ..2]
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0; 5.0; 6.0; 7.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - from specific index to end`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Get last 2 rows, last 2 cols: mat.[1.., 1..]
        let slice = mat.[1.., 1..]
        Assert.Equal(2, slice.NumRows)
        Assert.Equal(2, slice.NumCols)
        Assert.Equal<Vector<float>>([|5.0; 6.0; 8.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - single row range`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Extract middle row: mat.[1..1, 0..]
        let slice = mat.[1..1, 0..]
        Assert.Equal(1, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|4.0; 5.0; 6.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - single column range`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Extract middle column: mat.[0.., 1..1]
        let slice = mat.[0.., 1..1]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(1, slice.NumCols)
        Assert.Equal<Vector<float>>([|2.0; 5.0; 8.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - mixed start and end operators`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
            [|13.0; 14.0; 15.0; 16.0|]
        |]
        // Rows from start to 2, cols from 1 to end: mat.[..2, 1..]
        let slice = mat.[..2, 1..]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|2.0; 3.0; 4.0; 6.0; 7.0; 8.0; 10.0; 11.0; 12.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - first row only`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // First row: mat.[0..0, 0..]
        let slice = mat.[0..0, 0..]
        Assert.Equal(1, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - last row only`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Last row: mat.[2..2, 0..]
        let slice = mat.[2..2, 0..]
        Assert.Equal(1, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        Assert.Equal<Vector<float>>([|7.0; 8.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - first column only`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // First column: mat.[0.., 0..0]
        let slice = mat.[0.., 0..0]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(1, slice.NumCols)
        Assert.Equal<Vector<float>>([|1.0; 4.0; 7.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - last column only`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Last column: mat.[0.., 2..2]
        let slice = mat.[0.., 2..2]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(1, slice.NumCols)
        Assert.Equal<Vector<float>>([|3.0; 6.0; 9.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - tall matrix slice`` () =
        let mat = Matrix.init<float> 8 4 (fun i j -> float (i * 4 + j))
        // Get rows 2 to 5, cols 1 to 3: mat.[2..5, 1..3]
        let slice = mat.[2..5, 1..3]
        Assert.Equal(4, slice.NumRows)
        Assert.Equal(3, slice.NumCols)
        // Verify first row of slice (row 2 of original, cols 1-3)
        Assert.Equal(9.0, slice.[0, 0])   // mat.[2, 1]
        Assert.Equal(10.0, slice.[0, 1])  // mat.[2, 2]
        Assert.Equal(11.0, slice.[0, 2])  // mat.[2, 3]

    [<Fact>]
    let ``GetSlice Range - wide matrix slice`` () =
        let mat = Matrix.init<float> 3 10 (fun i j -> float (i * 10 + j))
        // Get all rows, cols 3 to 7: mat.[0.., 3..7]
        let slice = mat.[0.., 3..7]
        Assert.Equal(3, slice.NumRows)
        Assert.Equal(5, slice.NumCols)
        // Verify first row
        let firstRow = slice.[0..0, 0..]
        Assert.Equal<Vector<float>>([|3.0; 4.0; 5.0; 6.0; 7.0|], firstRow.Data)

    [<Fact>]
    let ``GetSlice Range - single element as 1x1 matrix`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0|]
            [|4.0; 5.0; 6.0|]
            [|7.0; 8.0; 9.0|]
        |]
        // Extract center element as 1x1 matrix: mat.[1..1, 1..1]
        let slice = mat.[1..1, 1..1]
        Assert.Equal(1, slice.NumRows)
        Assert.Equal(1, slice.NumCols)
        Assert.Equal<Vector<float>>([|5.0|], slice.Data)

    [<Fact>]
    let ``GetSlice Range - corner submatrices`` () =
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
            [|13.0; 14.0; 15.0; 16.0|]
        |]
        // Top-left 2x2: mat.[..1, ..1]
        let topLeft = mat.[..1, ..1]
        Assert.Equal<Vector<float>>([|1.0; 2.0; 5.0; 6.0|], topLeft.Data)

        // Top-right 2x2: mat.[..1, 2..]
        let topRight = mat.[..1, 2..]
        Assert.Equal<Vector<float>>([|3.0; 4.0; 7.0; 8.0|], topRight.Data)

        // Bottom-left 2x2: mat.[2.., ..1]
        let bottomLeft = mat.[2.., ..1]
        Assert.Equal<Vector<float>>([|9.0; 10.0; 13.0; 14.0|], bottomLeft.Data)

        // Bottom-right 2x2: mat.[2.., 2..]
        let bottomRight = mat.[2.., 2..]
        Assert.Equal<Vector<float>>([|11.0; 12.0; 15.0; 16.0|], bottomRight.Data)
