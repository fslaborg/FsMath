namespace FsMath.Tests.Matrix

open System
open Xunit
open FsMath
open FsMath.Tests
open FsMath.Tests.AssertHelpers

/// <summary>
/// Comprehensive tests for Matrix.multiplyRowVector SIMD paths.
/// These tests specifically target the SIMD-optimized code path (lines 609-636 in Matrix.fs)
/// which is activated when:
/// 1. Vector.IsHardwareAccelerated is true
/// 2. Matrix has at least Vector&lt;'T&gt;.Count columns (typically 4 for float)
/// </summary>
module MatrixSIMDMultiplyRowVectorTests =

    [<Fact>]
    let ``multiplyRowVector SIMD path - 3x4 matrix exactly SIMD width`` () =
        // Matrix with exactly 4 columns (Vector<float>.Count) to trigger SIMD
        let v = [|1.0; 2.0; 3.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
        |]
        // v × M = 1*[1,2,3,4] + 2*[5,6,7,8] + 3*[9,10,11,12]
        //       = [1,2,3,4] + [10,12,14,16] + [27,30,33,36]
        //       = [38, 44, 50, 56]
        let result = v * mat
        floatArrayClose [|38.0; 44.0; 50.0; 56.0|] result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - 2x8 matrix double SIMD width`` () =
        // Matrix with 8 columns (2× Vector<float>.Count) for full SIMD utilization
        let v = [|0.5; 1.5|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0; 13.0; 14.0; 15.0; 16.0|]
        |]
        // v × M = 0.5*row0 + 1.5*row1
        let result = v * mat
        let expected = [|14.0; 16.0; 18.0; 20.0; 22.0; 24.0; 26.0; 28.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - 4x16 matrix large SIMD`` () =
        // Large matrix with 16 columns for extensive SIMD processing
        let v = [|1.0; 2.0; 3.0; 4.0|]
        let mat = Matrix.init<float> 4 16 (fun i j -> float (i * 16 + j + 1))
        let result = v * mat

        // Manually compute expected
        let mutable expected = Array.zeroCreate<float> 16
        for i in 0..3 do
            for j in 0..15 do
                expected.[j] <- expected.[j] + v.[i] * mat.[i, j]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - with zero weights in SIMD`` () =
        // Test zero-weight optimization (line 620) in SIMD path
        let v = [|1.0; 0.0; 0.0; 2.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0; 5.0|]
            [|10.0; 20.0; 30.0; 40.0; 50.0|]
            [|100.0; 200.0; 300.0; 400.0; 500.0|]
            [|5.0; 6.0; 7.0; 8.0; 9.0|]
        |]
        // Only rows 0 and 3 contribute: 1*row0 + 2*row3
        let result = v * mat
        let expected = [|11.0; 14.0; 17.0; 20.0; 23.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - with scalar tail elements`` () =
        // 5 columns: 4 SIMD + 1 scalar tail (tests lines 632-634)
        let v = [|2.0; 3.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0; 5.0|]
            [|6.0; 7.0; 8.0; 9.0; 10.0|]
        |]
        // v × M = 2*row0 + 3*row1 = [2,4,6,8,10] + [18,21,24,27,30] = [20,25,30,35,40]
        let result = v * mat
        floatArrayClose [|20.0; 25.0; 30.0; 35.0; 40.0|] result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - 10 rows many weights`` () =
        // Many rows to test accumulation loop (lines 618-634)
        let v = [|1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0|]
        let mat = Matrix.init<float> 10 8 (fun i j -> float (i + j))
        let result = v * mat

        // Each column j should sum to: sum(i + j for i in 0..9) = 10j + 45
        let expected = Array.init 8 (fun j -> float (10 * j + 45))
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - negative weights`` () =
        // Test SIMD with negative weights
        let v = [|-1.0; 2.0; -0.5|]
        let mat = matrix [|
            [|4.0; 8.0; 12.0; 16.0|]
            [|2.0; 4.0; 6.0; 8.0|]
            [|10.0; 20.0; 30.0; 40.0|]
        |]
        // v × M = -1*row0 + 2*row1 + (-0.5)*row2
        let result = v * mat
        let expected = [|-5.0; -10.0; -15.0; -20.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - mixed precision float32`` () =
        // Test SIMD path with float32 (Vector<float32>.Count = 8)
        let v = [|1.0f; 2.0f; 3.0f|]
        let mat = Matrix<float32>(3, 8, [|
            1.0f; 2.0f; 3.0f; 4.0f; 5.0f; 6.0f; 7.0f; 8.0f;
            9.0f; 10.0f; 11.0f; 12.0f; 13.0f; 14.0f; 15.0f; 16.0f;
            17.0f; 18.0f; 19.0f; 20.0f; 21.0f; 22.0f; 23.0f; 24.0f
        |])
        let result = v * mat
        // v × M = 1*row0 + 2*row1 + 3*row2
        let expected = [|70.0f; 76.0f; 82.0f; 88.0f; 94.0f; 100.0f; 106.0f; 112.0f|]
        Assert.Equal<Vector<float32>>(expected, result)

    [<Fact>]
    let ``multiplyRowVector SIMD path - fractional weights`` () =
        // Test SIMD with fractional weights (common in probability/statistics)
        let v = [|0.25; 0.25; 0.25; 0.25|]  // Uniform weights
        let mat = matrix [|
            [|4.0; 8.0; 12.0; 16.0|]
            [|20.0; 24.0; 28.0; 32.0|]
            [|36.0; 40.0; 44.0; 48.0|]
            [|52.0; 56.0; 60.0; 64.0|]
        |]
        // Average of rows: (row0 + row1 + row2 + row3) / 4
        let result = v * mat
        let expected = [|28.0; 32.0; 36.0; 40.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - very large matrix 20x32`` () =
        // Large matrix to stress test SIMD loops
        let v = Array.init 20 (fun i -> float (i + 1))
        let mat = Matrix.init<float> 20 32 (fun i j -> float ((i * 32 + j) % 100))
        let result = v * mat

        // Verify dimensions and non-zero result
        Assert.Equal(32, result.Length)
        Assert.True(Array.exists (fun x -> x <> 0.0) result)

        // Verify mathematical correctness for a few columns
        for j in [0; 15; 31] do
            let mutable sum = 0.0
            for i in 0..19 do
                sum <- sum + v.[i] * mat.[i, j]
            Assert.InRange(abs(result.[j] - sum), 0.0, 1e-9)

    [<Fact>]
    let ``multiplyRowVector SIMD path - all one weights`` () =
        // Sum all rows with weight 1.0
        let v = [|1.0; 1.0; 1.0; 1.0; 1.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
            [|9.0; 10.0; 11.0; 12.0|]
            [|13.0; 14.0; 15.0; 16.0|]
            [|17.0; 18.0; 19.0; 20.0|]
        |]
        // Sum of all rows
        let result = v * mat
        let expected = [|45.0; 50.0; 55.0; 60.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - identity-like weights`` () =
        // One-hot vector (only one non-zero weight)
        let v = [|0.0; 0.0; 1.0; 0.0; 0.0|]
        let mat = Matrix.init<float> 5 4 (fun i j -> float (i * 4 + j + 1))
        let result = v * mat
        // Should return row 2 (0-indexed)
        let expected = [|9.0; 10.0; 11.0; 12.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - alternating positive negative`` () =
        // Alternating sign weights
        let v = [|1.0; -1.0; 1.0; -1.0|]
        let mat = matrix [|
            [|2.0; 4.0; 6.0; 8.0|]
            [|1.0; 3.0; 5.0; 7.0|]
            [|10.0; 12.0; 14.0; 16.0|]
            [|9.0; 11.0; 13.0; 15.0|]
        |]
        // v × M = row0 - row1 + row2 - row3
        let result = v * mat
        let expected = [|2.0; 2.0; 2.0; 2.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - 7 columns with tail`` () =
        // 7 columns: 4 SIMD + 3 scalar tail elements
        let v = [|1.0; 2.0; 3.0|]
        let mat = matrix [|
            [|1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0|]
            [|2.0; 2.0; 2.0; 2.0; 2.0; 2.0; 2.0|]
            [|3.0; 3.0; 3.0; 3.0; 3.0; 3.0; 3.0|]
        |]
        // v × M = 1*row0 + 2*row1 + 3*row2 = [1+4+9, ...] = [14, 14, 14, 14, 14, 14, 14]
        let result = v * mat
        floatArrayClose [|14.0; 14.0; 14.0; 14.0; 14.0; 14.0; 14.0|] result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - 9 columns multiple chunks`` () =
        // 9 columns: 2 SIMD chunks (8 elements) + 1 scalar tail
        let v = [|0.5; 1.5|]
        let mat = Matrix.init<float> 2 9 (fun i j -> float (i * 9 + j + 1))
        let result = v * mat
        // v × M = 0.5*[1..9] + 1.5*[10..18]
        let expected = Array.init 9 (fun j -> 0.5 * float (j + 1) + 1.5 * float (j + 10))
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - large weights`` () =
        // Test with large magnitude weights
        let v = [|1000.0; 2000.0; 3000.0|]
        let mat = matrix [|
            [|0.001; 0.002; 0.003; 0.004|]
            [|0.005; 0.006; 0.007; 0.008|]
            [|0.009; 0.010; 0.011; 0.012|]
        |]
        let result = v * mat
        // v × M = [1000*0.001 + 2000*0.005 + 3000*0.009, ...]
        let expected = [|38.0; 44.0; 50.0; 56.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - small weights near zero`` () =
        // Test numerical stability with very small weights
        let v = [|1e-10; 2e-10; 3e-10|]
        let mat = matrix [|
            [|1e10; 2e10; 3e10; 4e10|]
            [|5e10; 6e10; 7e10; 8e10|]
            [|9e10; 10e10; 11e10; 12e10|]
        |]
        // Result should be moderate magnitude
        let result = v * mat
        let expected = [|38.0; 44.0; 50.0; 56.0|]
        floatArrayClose expected result 1e-6

    [<Fact>]
    let ``multiplyRowVector SIMD path - sparse weights mostly zeros`` () =
        // Sparse vector with mostly zeros (tests zero-weight skip optimization)
        let v = [|0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 2.0; 0.0|]
        let mat = Matrix.init<float> 8 4 (fun i j -> float (i * 4 + j + 1))
        let result = v * mat
        // Only rows 3 and 6 contribute: 1*row3 + 2*row6
        // row3 = [13, 14, 15, 16], row6 = [25, 26, 27, 28]
        // expected = [13 + 50, 14 + 52, 15 + 54, 16 + 56] = [63, 66, 69, 72]
        let expected = [|63.0; 66.0; 69.0; 72.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - 12 columns three SIMD chunks`` () =
        // 12 columns = 3 × 4 (Vector<float>.Count), perfect SIMD alignment
        let v = [|1.0; 1.0|]
        let mat = Matrix.init<float> 2 12 (fun i j -> float (i * 12 + j + 1))
        let result = v * mat
        // Sum of two rows
        let expected = Array.init 12 (fun j -> float (j + 1) + float (j + 13))
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - single row with SIMD width`` () =
        // Single row vector × matrix (weight = 5.0)
        let v = [|5.0|]
        let mat = matrix [|[|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0|]|]
        let result = v * mat
        let expected = [|5.0; 10.0; 15.0; 20.0; 25.0; 30.0; 35.0; 40.0|]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyRowVector SIMD path - equals operator shorthand`` () =
        // Test using the * operator overload
        let v = [|2.0; 3.0|]
        let mat = matrix [|
            [|1.0; 2.0; 3.0; 4.0|]
            [|5.0; 6.0; 7.0; 8.0|]
        |]
        let result1 = v * mat
        let result2 = Matrix.multiplyRowVector v mat
        Assert.Equal<Vector<float>>(result1, result2)
