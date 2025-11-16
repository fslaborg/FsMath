namespace FsMath.Tests.VectorOps

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle

/// Comprehensive tests for VectorOps operators and type classes
module VectorOpsTests =

    // =============================================
    // Power Operator Tests (.^)
    // =============================================

    [<Fact>]
    let ``power operator .^ raises each element to power (float)`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = v .^ 2.0
        let expected = [| 4.0; 9.0; 16.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``power operator .^ with fractional power (float)`` () =
        let v = [| 4.0; 9.0; 16.0 |]
        let result = v .^ 0.5
        let expected = [| 2.0; 3.0; 4.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``power operator .^ with zero power returns ones (float)`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = v .^ 0.0
        let expected = [| 1.0; 1.0; 1.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``power operator .^ with negative power (float)`` () =
        let v = [| 2.0; 4.0; 5.0 |]
        let result = v .^ -1.0
        let expected = [| 0.5; 0.25; 0.2 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``power operator with integer power (float)`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = v .^ 3.0
        let expected = [| 8.0; 27.0; 64.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Dot Product Tests - REMOVED: These tests are duplicated 
    // in VectorFloatTests.fs and VectorIntTests.fs
    // =============================================

    // =============================================
    // Scalar-Vector Operators (Scalar on Left)
    // =============================================

    [<Fact>]
    let ``scalar-vector addition: scalar .+ vector (float)`` () =
        let scalar = 10.0
        let v = [| 1.0; 2.0; 3.0 |]
        let result = scalar .+ v
        let expected = [| 11.0; 12.0; 13.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``scalar-vector multiplication: scalar .* vector (float)`` () =
        let scalar = 2.0
        let v = [| 1.0; 2.0; 3.0 |]
        let result = scalar .* v
        let expected = [| 2.0; 4.0; 6.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``scalar-vector multiplication: scalar .* vector (int)`` () =
        let scalar = 3
        let v = [| 1; 2; 3 |]
        let result = scalar .* v
        let expected = [| 3; 6; 9 |]
        Assert.Equal<Vector<int>>(expected, result)

    // =============================================
    // Vector-Scalar Operators (Comprehensive)
    // =============================================

    [<Fact>]
    let ``vector .+ scalar (int)`` () =
        let v = [| 1; 2; 3 |]
        let result = v .+ 10
        let expected = [| 11; 12; 13 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``vector .- scalar (int)`` () =
        let v = [| 10; 20; 30 |]
        let result = v .- 5
        let expected = [| 5; 15; 25 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``vector .* scalar (int)`` () =
        let v = [| 2; 3; 4 |]
        let result = v .* 3
        let expected = [| 6; 9; 12 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``vector ./ scalar (int)`` () =
        let v = [| 10; 20; 30 |]
        let result = v ./ 5
        let expected = [| 2; 4; 6 |]
        Assert.Equal<Vector<int>>(expected, result)

    // =============================================
    // Vector-Vector Operators (Comprehensive)
    // =============================================

    [<Fact>]
    let ``vector .+ vector (int)`` () =
        let v1 = [| 1; 2; 3 |]
        let v2 = [| 4; 5; 6 |]
        let result = v1 .+ v2
        let expected = [| 5; 7; 9 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``vector .- vector (int)`` () =
        let v1 = [| 10; 20; 30 |]
        let v2 = [| 1; 2; 3 |]
        let result = v1 .- v2
        let expected = [| 9; 18; 27 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``vector .* vector (int)`` () =
        let v1 = [| 2; 3; 4 |]
        let v2 = [| 5; 6; 7 |]
        let result = v1 .* v2
        let expected = [| 10; 18; 28 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``vector ./ vector (int)`` () =
        let v1 = [| 20; 30; 40 |]
        let v2 = [| 2; 3; 4 |]
        let result = v1 ./ v2
        let expected = [| 10; 10; 10 |]
        Assert.Equal<Vector<int>>(expected, result)

    // =============================================
    // Edge Cases
    // =============================================

    [<Fact>]
    let ``operators work with single-element vectors (float)`` () =
        let v1 = [| 5.0 |]
        let v2 = [| 3.0 |]

        let addResult = v1 .+ v2
        Assert.Equal<Vector<float>>([| 8.0 |], addResult)

        let mulResult = v1 .* v2
        Assert.Equal<Vector<float>>([| 15.0 |], mulResult)

        let dotResult = Vector.dot v1 v2
        floatEqual 15.0 dotResult 1e-10

    [<Fact>]
    let ``operators work with large vectors (float)`` () =
        let size = 1000
        let v1 = Array.init size (fun i -> float i)
        let v2 = Array.init size (fun i -> float (i + 1))

        let addResult = v1 .+ v2
        Assert.Equal(size, addResult.Length)
        floatEqual (float (size - 1) + float size) addResult.[size - 1] 1e-10

        let mulResult = v1 .* v2
        Assert.Equal(size, mulResult.Length)

    [<Fact>]
    let ``vector-vector addition throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> (v1 .+ v2) |> ignore)

    [<Fact>]
    let ``vector-vector subtraction throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> (v1 .- v2) |> ignore)

    [<Fact>]
    let ``vector-vector multiplication throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> (v1 .* v2) |> ignore)

    [<Fact>]
    let ``vector-vector division throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> (v1 ./ v2) |> ignore)

    // =============================================
    // Mixed Operations (Combining Operators)
    // =============================================

    [<Fact>]
    let ``combined operations: (v1 .+ v2) .* scalar`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = (v1 .+ v2) .* 2.0
        let expected = [| 10.0; 14.0; 18.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``combined operations: dot product of scaled vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let scaled = v .* 2.0
        let result = Vector.dot scaled v
        // (2*1)*1 + (2*2)*2 + (2*3)*3 = 2 + 8 + 18 = 28
        floatEqual 28.0 result 1e-10

    [<Fact>]
    let ``combined operations: (v1 .^ 2) .+ (v2 .^ 2)`` () =
        let v1 = [| 3.0; 4.0 |]
        let v2 = [| 0.0; 0.0 |]
        let result = (v1 .^ 2.0) .+ (v2 .^ 2.0)
        let expected = [| 9.0; 16.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Zero and Special Values
    // =============================================

    [<Fact>]
    let ``operations with zero vector (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let zero = [| 0.0; 0.0; 0.0 |]

        let addResult = v .+ zero
        floatArrayClose v addResult 1e-10

        let mulResult = v .* zero
        floatArrayClose zero mulResult 1e-10

        let dotResult = Vector.dot v zero
        floatEqual 0.0 dotResult 1e-10

    [<Fact>]
    let ``multiplication by zero scalar (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = v .* 0.0
        let expected = [| 0.0; 0.0; 0.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``addition with negative values (float)`` () =
        let v1 = [| 1.0; -2.0; 3.0 |]
        let v2 = [| -1.0; 2.0; -3.0 |]
        let result = v1 .+ v2
        let expected = [| 0.0; 0.0; 0.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Commutativity and Algebraic Properties
    // =============================================

    [<Fact>]
    let ``addition is commutative (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result1 = v1 .+ v2
        let result2 = v2 .+ v1
        floatArrayClose result1 result2 1e-10

    [<Fact>]
    let ``multiplication is commutative (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result1 = v1 .* v2
        let result2 = v2 .* v1
        floatArrayClose result1 result2 1e-10

    [<Fact>]
    let ``dot product is commutative (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result1 = Vector.dot v1 v2
        let result2 = Vector.dot v2 v1
        floatEqual result1 result2 1e-10

    [<Fact>]
    let ``scalar multiplication distributes over addition (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let scalar = 2.5
        let result1 = scalar .* (v1 .+ v2)
        let result2 = (scalar .* v1) .+ (scalar .* v2)
        floatArrayClose result1 result2 1e-10

