namespace FsMath.Tests.Matrix

open Xunit
open FsMath

/// <summary>
/// Comprehensive SIMD coverage tests for Matrix vector operations.
/// These tests specifically target SIMD code paths in muliplyVector, addRowVector, and addColVector
/// to improve coverage for lines 496, 534, and 572 in Matrix.fs.
///
/// Key Requirements for SIMD Activation:
/// - Vector&lt;float&gt;.Count is typically 4 on most systems
/// - Matrix columns must be >= 4 for SIMD to activate
/// - Tests include both SIMD chunks and scalar tail processing
/// </summary>
module MatrixSIMDCoverageTests =

    // =========================================================================
    // Tests for Matrix.muliplyVector (line 496: SIMD accumulation loop)
    // =========================================================================

    [<Fact>]
    let ``muliplyVector: 2x4 matrix with float - triggers SIMD path`` () =
        // 4 columns = exactly 1 SIMD chunk (Vector<float>.Count = 4)
        let mat = Matrix.create 2 4 [|1.0; 2.0; 3.0; 4.0;
                                       5.0; 6.0; 7.0; 8.0|]
        let vec = [|1.0; 2.0; 3.0; 4.0|]
        let result = Matrix.muliplyVector mat vec

        // Expected: [1*1 + 2*2 + 3*3 + 4*4; 5*1 + 6*2 + 7*3 + 8*4]
        //         = [30.0; 70.0]
        Assert.Equal<float>(30.0, result.[0])
        Assert.Equal<float>(70.0, result.[1])

    [<Fact>]
    let ``muliplyVector: 3x8 matrix with float - multiple SIMD chunks`` () =
        // 8 columns = 2 SIMD chunks
        let mat = Matrix.create 3 8 [|1.0; 1.0; 1.0; 1.0; 2.0; 2.0; 2.0; 2.0;
                                       3.0; 3.0; 3.0; 3.0; 4.0; 4.0; 4.0; 4.0;
                                       5.0; 5.0; 5.0; 5.0; 6.0; 6.0; 6.0; 6.0|]
        let vec = [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0|]
        let result = Matrix.muliplyVector mat vec

        // Row 0: 1*(1+2+3+4) + 2*(5+6+7+8) = 10 + 52 = 62
        // Row 1: 3*(1+2+3+4) + 4*(5+6+7+8) = 30 + 104 = 134
        // Row 2: 5*(1+2+3+4) + 6*(5+6+7+8) = 50 + 156 = 206
        Assert.Equal<float>(62.0, result.[0])
        Assert.Equal<float>(134.0, result.[1])
        Assert.Equal<float>(206.0, result.[2])

    [<Fact>]
    let ``muliplyVector: 2x5 matrix with float - SIMD + scalar tail`` () =
        // 5 columns = 1 SIMD chunk (4 elements) + 1 scalar tail
        let mat = Matrix.create 2 5 [|1.0; 2.0; 3.0; 4.0; 5.0;
                                       6.0; 7.0; 8.0; 9.0; 10.0|]
        let vec = [|1.0; 1.0; 1.0; 1.0; 1.0|]
        let result = Matrix.muliplyVector mat vec

        // Row 0: 1+2+3+4+5 = 15
        // Row 1: 6+7+8+9+10 = 40
        Assert.Equal<float>(15.0, result.[0])
        Assert.Equal<float>(40.0, result.[1])

    [<Fact>]
    let ``muliplyVector: 4x12 matrix with float - extensive SIMD processing`` () =
        // 12 columns = 3 SIMD chunks
        let data = Array.init (4 * 12) (fun i -> float (i + 1))
        let mat = Matrix.create 4 12 data
        let vec = Array.create 12 1.0
        let result = Matrix.muliplyVector mat vec

        // Each row is the sum of 12 consecutive numbers
        Assert.Equal<float>(78.0, result.[0])   // sum(1..12)
        Assert.Equal<float>(222.0, result.[1])  // sum(13..24)
        Assert.Equal<float>(366.0, result.[2])  // sum(25..36)
        Assert.Equal<float>(510.0, result.[3])  // sum(37..48)

    [<Fact>]
    let ``muliplyVector: 2x4 with float32 - SIMD with different type`` () =
        // Vector<float32>.Count is typically 8, so 4 columns won't trigger SIMD
        // But this tests the function with float32 type
        let mat = Matrix.create 2 4 [|1.0f; 2.0f; 3.0f; 4.0f;
                                       5.0f; 6.0f; 7.0f; 8.0f|]
        let vec = [|1.0f; 2.0f; 3.0f; 4.0f|]
        let result = Matrix.muliplyVector mat vec

        Assert.Equal<float32>(30.0f, result.[0])
        Assert.Equal<float32>(70.0f, result.[1])

    [<Fact>]
    let ``muliplyVector: 3x16 matrix with float32 - multiple SIMD chunks float32`` () =
        // Vector<float32>.Count is typically 8, so 16 columns = 2 SIMD chunks
        let data = Array.init (3 * 16) (fun i -> float32 (i % 4 + 1))
        let mat = Matrix.create 3 16 data
        let vec = Array.create 16 1.0f
        let result = Matrix.muliplyVector mat vec

        // Each row: 4 cycles of [1,2,3,4] = 4*(1+2+3+4) = 40
        Assert.Equal<float32>(40.0f, result.[0])
        Assert.Equal<float32>(40.0f, result.[1])
        Assert.Equal<float32>(40.0f, result.[2])

    [<Fact>]
    let ``muliplyVector: identity matrix - SIMD path validation`` () =
        let n = 8
        let mat = Matrix.identity<float> n
        let vec = [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0|]
        let result = Matrix.muliplyVector mat vec

        // Identity matrix should return the same vector
        Assert.Equal<Vector<float>>(vec, result)

    [<Fact>]
    let ``muliplyVector: 4x6 zero matrix with SIMD`` () =
        let mat = Matrix.zeroCreate<float> 4 6
        let vec = [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0|]
        let result = Matrix.muliplyVector mat vec

        Assert.All(result, fun x -> Assert.Equal<float>(0.0, x))

    // =========================================================================
    // Tests for Matrix.addRowVector (line 534: SIMD addition loop)
    // =========================================================================

    [<Fact>]
    let ``addRowVector: 2x4 matrix - triggers SIMD path`` () =
        // 4 columns = exactly 1 SIMD chunk
        let mat = Matrix.create 2 4 [|1.0; 2.0; 3.0; 4.0;
                                       5.0; 6.0; 7.0; 8.0|]
        let vec = [|10.0; 20.0; 30.0; 40.0|]
        let result = Matrix.addRowVector mat vec

        let expected = [|11.0; 22.0; 33.0; 44.0; 15.0; 26.0; 37.0; 48.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addRowVector: 3x8 matrix - multiple SIMD chunks`` () =
        // 8 columns = 2 SIMD chunks
        let mat = Matrix.create 3 8 (Array.init 24 (fun i -> float (i + 1)))
        let vec = Array.create 8 100.0
        let result = Matrix.addRowVector mat vec

        // Each element should be increased by 100
        for i = 0 to 23 do
            Assert.Equal<float>(float (i + 1) + 100.0, result.Data.[i])

    [<Fact>]
    let ``addRowVector: 2x5 matrix - SIMD + scalar tail`` () =
        // 5 columns = 1 SIMD chunk + 1 scalar tail
        let mat = Matrix.create 2 5 [|1.0; 2.0; 3.0; 4.0; 5.0;
                                       6.0; 7.0; 8.0; 9.0; 10.0|]
        let vec = [|1.0; 1.0; 1.0; 1.0; 1.0|]
        let result = Matrix.addRowVector mat vec

        let expected = [|2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0; 11.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addRowVector: 4x12 matrix - extensive SIMD processing`` () =
        // 12 columns = 3 SIMD chunks
        let mat = Matrix.ones<float> 4 12
        let vec = Array.init 12 (fun i -> float i)
        let result = Matrix.addRowVector mat vec

        // Each row should have [1+0, 1+1, 1+2, ..., 1+11]
        for row = 0 to 3 do
            for col = 0 to 11 do
                Assert.Equal<float>(1.0 + float col, result.[row, col])

    [<Fact>]
    let ``addRowVector: 2x16 with float32 - SIMD with float32 type`` () =
        // Vector<float32>.Count is typically 8, so 16 columns = 2 SIMD chunks
        let mat = Matrix.create 2 16 (Array.init 32 (fun i -> float32 i))
        let vec = Array.create 16 10.0f
        let result = Matrix.addRowVector mat vec

        for i = 0 to 31 do
            Assert.Equal<float32>(float32 i + 10.0f, result.Data.[i])

    [<Fact>]
    let ``addRowVector: 3x6 with negative values`` () =
        let mat = Matrix.create 3 6 [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0;
                                       7.0; 8.0; 9.0; 10.0; 11.0; 12.0;
                                       13.0; 14.0; 15.0; 16.0; 17.0; 18.0|]
        let vec = [|-1.0; -2.0; -3.0; -4.0; -5.0; -6.0|]
        let result = Matrix.addRowVector mat vec

        let expected = [|0.0; 0.0; 0.0; 0.0; 0.0; 0.0;
                         6.0; 6.0; 6.0; 6.0; 6.0; 6.0;
                         12.0; 12.0; 12.0; 12.0; 12.0; 12.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addRowVector: 5x9 large matrix - mixed SIMD and tail`` () =
        // 9 columns = 2 SIMD chunks + 1 scalar tail (assuming Vector<float>.Count = 4)
        let mat = Matrix.zeroCreate<float> 5 9
        let vec = [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0|]
        let result = Matrix.addRowVector mat vec

        // Each row should equal vec
        for row = 0 to 4 do
            for col = 0 to 8 do
                Assert.Equal<float>(vec.[col], result.[row, col])

    // =========================================================================
    // Tests for Matrix.addColVector (line 572: SIMD addition with broadcast)
    // =========================================================================

    [<Fact>]
    let ``addColVector: 2x4 matrix - triggers SIMD path`` () =
        // 4 columns = exactly 1 SIMD chunk
        let mat = Matrix.create 2 4 [|1.0; 2.0; 3.0; 4.0;
                                       5.0; 6.0; 7.0; 8.0|]
        let vec = [|100.0; 200.0|]
        let result = Matrix.addColVector mat vec

        let expected = [|101.0; 102.0; 103.0; 104.0; 205.0; 206.0; 207.0; 208.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addColVector: 3x8 matrix - multiple SIMD chunks`` () =
        // 8 columns = 2 SIMD chunks
        let mat = Matrix.create 3 8 (Array.init 24 (fun i -> float (i + 1)))
        let vec = [|10.0; 20.0; 30.0|]
        let result = Matrix.addColVector mat vec

        // Row 0: add 10 to first 8 elements
        for i = 0 to 7 do
            Assert.Equal<float>(float (i + 1) + 10.0, result.Data.[i])
        // Row 1: add 20 to next 8 elements
        for i = 8 to 15 do
            Assert.Equal<float>(float (i + 1) + 20.0, result.Data.[i])
        // Row 2: add 30 to last 8 elements
        for i = 16 to 23 do
            Assert.Equal<float>(float (i + 1) + 30.0, result.Data.[i])

    [<Fact>]
    let ``addColVector: 2x5 matrix - SIMD + scalar tail`` () =
        // 5 columns = 1 SIMD chunk + 1 scalar tail
        let mat = Matrix.create 2 5 [|1.0; 2.0; 3.0; 4.0; 5.0;
                                       6.0; 7.0; 8.0; 9.0; 10.0|]
        let vec = [|10.0; 20.0|]
        let result = Matrix.addColVector mat vec

        let expected = [|11.0; 12.0; 13.0; 14.0; 15.0; 26.0; 27.0; 28.0; 29.0; 30.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addColVector: 4x12 matrix - extensive SIMD processing`` () =
        // 12 columns = 3 SIMD chunks
        let mat = Matrix.ones<float> 4 12
        let vec = [|0.0; 10.0; 20.0; 30.0|]
        let result = Matrix.addColVector mat vec

        // Row 0: all 1.0, Row 1: all 11.0, Row 2: all 21.0, Row 3: all 31.0
        for col = 0 to 11 do
            Assert.Equal<float>(1.0, result.[0, col])
            Assert.Equal<float>(11.0, result.[1, col])
            Assert.Equal<float>(21.0, result.[2, col])
            Assert.Equal<float>(31.0, result.[3, col])

    [<Fact>]
    let ``addColVector: 3x16 with float32 - SIMD with float32 type`` () =
        // Vector<float32>.Count is typically 8, so 16 columns = 2 SIMD chunks
        let mat = Matrix.zeroCreate<float32> 3 16
        let vec = [|1.0f; 2.0f; 3.0f|]
        let result = Matrix.addColVector mat vec

        for col = 0 to 15 do
            Assert.Equal<float32>(1.0f, result.[0, col])
            Assert.Equal<float32>(2.0f, result.[1, col])
            Assert.Equal<float32>(3.0f, result.[2, col])

    [<Fact>]
    let ``addColVector: 2x6 with negative column vector`` () =
        let mat = Matrix.create 2 6 [|10.0; 20.0; 30.0; 40.0; 50.0; 60.0;
                                       70.0; 80.0; 90.0; 100.0; 110.0; 120.0|]
        let vec = [|-5.0; -15.0|]
        let result = Matrix.addColVector mat vec

        let expected = [|5.0; 15.0; 25.0; 35.0; 45.0; 55.0;
                         55.0; 65.0; 75.0; 85.0; 95.0; 105.0|]
        Assert.Equal<Vector<float>>(expected, result.Data)

    [<Fact>]
    let ``addColVector: 5x9 large matrix - mixed SIMD and tail`` () =
        // 9 columns = 2 SIMD chunks + 1 scalar tail (assuming Vector<float>.Count = 4)
        let mat = Matrix.zeroCreate<float> 5 9
        let vec = [|1.0; 2.0; 3.0; 4.0; 5.0|]
        let result = Matrix.addColVector mat vec

        for row = 0 to 4 do
            for col = 0 to 8 do
                Assert.Equal<float>(vec.[row], result.[row, col])

    [<Fact>]
    let ``addColVector: 6x20 very large matrix - stress test SIMD`` () =
        // 20 columns = 5 SIMD chunks (assuming Vector<float>.Count = 4)
        let mat = Matrix.create 6 20 (Array.init 120 (fun i -> float i))
        let vec = [|100.0; 200.0; 300.0; 400.0; 500.0; 600.0|]
        let result = Matrix.addColVector mat vec

        for row = 0 to 5 do
            let expectedAddition = vec.[row]
            for col = 0 to 19 do
                let originalValue = float (row * 20 + col)
                Assert.Equal<float>(originalValue + expectedAddition, result.[row, col])
