namespace FsMath.Tests

open Xunit
open FsMath

module MatrixEdgeCaseTests =

    // ============================================
    // Tests for Matrix indexer setter (line 39)
    // ============================================

    [<Fact>]
    let ``Matrix setter throws on negative row index`` () =
        let m = Matrix<float>.zeroCreate 3 3
        Assert.Throws<System.ArgumentException>(fun () -> m.[-1, 0] <- 1.0) |> ignore

    [<Fact>]
    let ``Matrix setter throws on row index too large`` () =
        let m = Matrix<float>.zeroCreate 3 3
        Assert.Throws<System.ArgumentException>(fun () -> m.[3, 0] <- 1.0) |> ignore

    [<Fact>]
    let ``Matrix setter throws on negative column index`` () =
        let m = Matrix<float>.zeroCreate 3 3
        Assert.Throws<System.ArgumentException>(fun () -> m.[0, -1] <- 1.0) |> ignore

    [<Fact>]
    let ``Matrix setter throws on column index too large`` () =
        let m = Matrix<float>.zeroCreate 3 3
        Assert.Throws<System.ArgumentException>(fun () -> m.[0, 3] <- 1.0) |> ignore


    // ============================================
    // Tests for toFormattedString with custom floatFormat (line 272)
    // ============================================

    [<Fact>]
    let ``toFormattedString with custom floatFormat`` () =
        let m = Matrix<float>.init 2 2 (fun i j -> float (i + j) + 0.123456)
        let formatted = m.toFormattedString(floatFormat = "0.0000")
        // Should contain the 4-decimal format
        Assert.Contains("0.1235", formatted) // 0.123456 rounded to 4 decimals

    [<Fact>]
    let ``toFormattedString with custom precision floatFormat`` () =
        let m = Matrix<float>.init 2 2 (fun i j -> float (i + j) * 1.5)
        let formatted = m.toFormattedString(floatFormat = "0.000")
        // Should use the custom format
        Assert.Contains("1.500", formatted)


    // ============================================
    // Tests for toFormattedString truncation message (line 319)
    // ============================================

    [<Fact>]
    let ``toFormattedString shows truncation message for large matrices`` () =
        let m = Matrix<float>.init 20 20 (fun i j -> float (i * 10 + j))
        let formatted = m.toFormattedString(maxRows = 5, maxCols = 5)
        // Should contain truncation message
        Assert.Contains("truncated", formatted)
        Assert.Contains("(20x20)", formatted)

    [<Fact>]
    let ``toFormattedString shows truncation for rows exceeding maxRows`` () =
        let m = Matrix<int>.init 15 3 (fun i j -> i + j)
        let formatted = m.toFormattedString(maxRows = 5)
        Assert.Contains("truncated", formatted)

    [<Fact>]
    let ``toFormattedString shows truncation for cols exceeding maxCols`` () =
        let m = Matrix<int>.init 3 15 (fun i j -> i + j)
        let formatted = m.toFormattedString(maxCols = 5)
        Assert.Contains("truncated", formatted)


    // ============================================
    // Tests for muliplyVector SIMD path (line 496)
    // This tests the accumulation line within the SIMD loop
    // ============================================

    [<Fact>]
    let ``muliplyVector with large vectors exercises SIMD path`` () =
        // Create a matrix and vector large enough to use SIMD
        let size = 64  // Should be larger than SIMD vector size
        let m = Matrix<float>.init size size (fun i j -> if i = j then 1.0 else 0.0)  // Identity
        let v = Array.init size (fun i -> float (i + 1))

        let result = Matrix.muliplyVector m v

        // Identity matrix should return the same vector
        for i = 0 to size - 1 do
            Assert.Equal(float (i + 1), result.[i])

    [<Fact>]
    let ``muliplyVector with non-trivial matrix exercises SIMD`` () =
        let rows = 32
        let cols = 32
        let m = Matrix<float>.init rows cols (fun i j -> float ((i + 1) * (j + 1)))
        let v = Array.init cols (fun i -> 1.0)

        let result = Matrix.muliplyVector m v

        // Each row sum should be sum of (i+1)*j for j=1 to cols
        for i = 0 to rows - 1 do
            let expected = float ((i + 1) * cols * (cols + 1) / 2)
            Assert.Equal(expected, result.[i])


    // ============================================
    // Tests for addRowVector SIMD path (line 534)
    // ============================================

    [<Fact>]
    let ``addRowVector with large matrix exercises SIMD path`` () =
        let rows = 10
        let cols = 64  // Large enough for SIMD
        let m = Matrix<float>.zeroCreate rows cols
        let v = Array.init cols (fun i -> float i)

        let result = Matrix.addRowVector m v

        // Every row should be equal to v
        for i = 0 to rows - 1 do
            for j = 0 to cols - 1 do
                Assert.Equal(float j, result.[i, j])

    [<Fact>]
    let ``addRowVector with varying row data`` () =
        let rows = 5
        let cols = 48
        let m = Matrix<float>.init rows cols (fun i j -> float (i * 10))
        let v = Array.init cols (fun i -> float (i + 1))

        let result = Matrix.addRowVector m v

        // Each row should have base value + v
        for i = 0 to rows - 1 do
            for j = 0 to cols - 1 do
                Assert.Equal(float (i * 10 + j + 1), result.[i, j])


    // ============================================
    // Tests for addColVector SIMD path (line 572)
    // ============================================

    [<Fact>]
    let ``addColVector with large matrix exercises SIMD path`` () =
        let rows = 16
        let cols = 64  // Large enough for SIMD
        let m = Matrix<float>.zeroCreate rows cols
        let v = Array.init rows (fun i -> float i)

        let result = Matrix.addColVector m v

        // Every element in row i should be equal to v.[i]
        for i = 0 to rows - 1 do
            for j = 0 to cols - 1 do
                Assert.Equal(float i, result.[i, j])

    [<Fact>]
    let ``addColVector with varying column data`` () =
        let rows = 8
        let cols = 48
        let m = Matrix<float>.init rows cols (fun i j -> float (j * 5))
        let v = Array.init rows (fun i -> float (i + 1))

        let result = Matrix.addColVector m v

        // Each element should have base value + v.[i]
        for i = 0 to rows - 1 do
            for j = 0 to cols - 1 do
                Assert.Equal(float (j * 5 + i + 1), result.[i, j])

    [<Fact>]
    let ``addColVector with int32 type`` () =
        let rows = 6
        let cols = 32
        let m = Matrix<int>.zeroCreate rows cols
        let v = Array.init rows (fun i -> i + 10)

        let result = Matrix.addColVector m v

        for i = 0 to rows - 1 do
            for j = 0 to cols - 1 do
                Assert.Equal(i + 10, result.[i, j])
