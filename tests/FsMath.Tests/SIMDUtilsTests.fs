namespace FsMath.Tests.SIMDUtils

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle

/// Comprehensive tests for SIMDUtils module
module SIMDUtilsTests =

    // =============================================
    // map Tests
    // =============================================

    [<Fact>]
    let ``map with identity function returns identical array (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |]
        let result = SIMDUtils.map (fun v -> v) (fun x -> x) input
        floatArrayClose input result 1e-10

    [<Fact>]
    let ``map doubles each element (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |]
        let result = SIMDUtils.map (fun v -> v * 2.0) (fun x -> x * 2.0) input
        let expected = [| 2.0; 4.0; 6.0; 8.0; 10.0; 12.0; 14.0; 16.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map adds constant to each element (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = SIMDUtils.map (fun v -> v + Numerics.Vector(5.0)) (fun x -> x + 5.0) input
        let expected = [| 6.0; 7.0; 8.0; 9.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map with negation (float)`` () =
        let input = [| 1.0; -2.0; 3.0; -4.0; 5.0 |]
        let result = SIMDUtils.map (fun v -> -v) (fun x -> -x) input
        let expected = [| -1.0; 2.0; -3.0; 4.0; -5.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map works with single element array (float)`` () =
        let input = [| 42.0 |]
        let result = SIMDUtils.map (fun v -> v * 2.0) (fun x -> x * 2.0) input
        let expected = [| 84.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map works with empty array (float)`` () =
        let input = [||] : float[]
        let result = SIMDUtils.map (fun v -> v * 2.0) (fun x -> x * 2.0) input
        Assert.Equal(0, result.Length)

    [<Fact>]
    let ``map works with large array (float)`` () =
        let size = 1000
        let input = Array.init size (fun i -> float i)
        let result = SIMDUtils.map (fun v -> v + Numerics.Vector(1.0)) (fun x -> x + 1.0) input
        let expected = Array.init size (fun i -> float (i + 1))
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map with integer type (int)`` () =
        let input = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let result = SIMDUtils.map (fun v -> v * 2) (fun x -> x * 2) input
        let expected = [| 2; 4; 6; 8; 10; 12; 14; 16 |]
        intArrayEqual expected result

    [<Fact>]
    let ``map handles non-SIMD-aligned sizes (float)`` () =
        // Test with size that doesn't divide evenly by SIMD width
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = SIMDUtils.map (fun v -> v * 3.0) (fun x -> x * 3.0) input
        let expected = [| 3.0; 6.0; 9.0; 12.0; 15.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // map2Unchecked Tests
    // =============================================

    [<Fact>]
    let ``map2Unchecked adds two arrays (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |]
        let v2 = [| 8.0; 7.0; 6.0; 5.0; 4.0; 3.0; 2.0; 1.0 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a + b) (fun a b -> a + b) v1 v2
        let expected = [| 9.0; 9.0; 9.0; 9.0; 9.0; 9.0; 9.0; 9.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map2Unchecked multiplies two arrays (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0; 4.0 |]
        let v2 = [| 2.0; 3.0; 4.0; 5.0 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a * b) (fun a b -> a * b) v1 v2
        let expected = [| 2.0; 6.0; 12.0; 20.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map2Unchecked subtracts arrays (float)`` () =
        let v1 = [| 10.0; 20.0; 30.0; 40.0 |]
        let v2 = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a - b) (fun a b -> a - b) v1 v2
        let expected = [| 9.0; 18.0; 27.0; 36.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map2Unchecked divides arrays (float)`` () =
        let v1 = [| 10.0; 20.0; 30.0; 40.0 |]
        let v2 = [| 2.0; 4.0; 5.0; 8.0 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a / b) (fun a b -> a / b) v1 v2
        let expected = [| 5.0; 5.0; 6.0; 5.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map2Unchecked with single element (float)`` () =
        let v1 = [| 3.0 |]
        let v2 = [| 7.0 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a + b) (fun a b -> a + b) v1 v2
        let expected = [| 10.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map2Unchecked with empty arrays (float)`` () =
        let v1 = [||] : float[]
        let v2 = [||] : float[]
        let result = SIMDUtils.map2Unchecked (fun a b -> a + b) (fun a b -> a + b) v1 v2
        Assert.Equal(0, result.Length)

    [<Fact>]
    let ``map2Unchecked with integers (int)`` () =
        let v1 = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let v2 = [| 8; 7; 6; 5; 4; 3; 2; 1 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a + b) (fun a b -> a + b) v1 v2
        let expected = [| 9; 9; 9; 9; 9; 9; 9; 9 |]
        intArrayEqual expected result

    [<Fact>]
    let ``map2Unchecked with large arrays (float)`` () =
        let size = 1000
        let v1 = Array.init size (fun i -> float i)
        let v2 = Array.init size (fun i -> float (i + 1))
        let result = SIMDUtils.map2Unchecked (fun a b -> a + b) (fun a b -> a + b) v1 v2
        let expected = Array.init size (fun i -> float (2 * i + 1))
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``map2Unchecked handles non-SIMD-aligned sizes (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let v2 = [| 5.0; 4.0; 3.0; 2.0; 1.0 |]
        let result = SIMDUtils.map2Unchecked (fun a b -> a * b) (fun a b -> a * b) v1 v2
        let expected = [| 5.0; 8.0; 9.0; 8.0; 5.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // fold Tests
    // =============================================

    [<Fact>]
    let ``fold sums all elements (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |]
        let result = SIMDUtils.fold (fun a b -> a + b) (fun a b -> a + b) 0.0 input
        floatEqual 36.0 result 1e-10

    [<Fact>]
    let ``fold multiplies all elements (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = SIMDUtils.fold (fun a b -> a * b) (fun a b -> a * b) 1.0 input
        floatEqual 24.0 result 1e-10

    [<Fact>]
    let ``fold finds maximum (float)`` () =
        let input = [| 3.0; 1.0; 4.0; 1.0; 5.0; 9.0; 2.0; 6.0 |]
        let result = SIMDUtils.fold (fun a b -> Numerics.Vector.Max(a, b)) (fun a b -> max a b) System.Double.NegativeInfinity input
        floatEqual 9.0 result 1e-10

    [<Fact>]
    let ``fold with single element (float)`` () =
        let input = [| 42.0 |]
        let result = SIMDUtils.fold (fun a b -> a + b) (fun a b -> a + b) 0.0 input
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``fold with empty array returns zero (float)`` () =
        let input = [||] : float[]
        let result = SIMDUtils.fold (fun a b -> a + b) (fun a b -> a + b) 0.0 input
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``fold sum with integers (int)`` () =
        let input = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let result = SIMDUtils.fold (fun a b -> a + b) (fun a b -> a + b) 0 input
        intEqual 36 result

    [<Fact>]
    let ``fold with large array (float)`` () =
        let size = 1000
        let input = Array.init size (fun i -> 1.0)
        let result = SIMDUtils.fold (fun a b -> a + b) (fun a b -> a + b) 0.0 input
        floatEqual (float size) result 1e-8

    [<Fact>]
    let ``fold handles non-SIMD-aligned sizes (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = SIMDUtils.fold (fun a b -> a + b) (fun a b -> a + b) 0.0 input
        floatEqual 15.0 result 1e-10

    // =============================================
    // mapScalar Tests
    // =============================================

    [<Fact>]
    let ``mapScalar adds scalar to each element (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v + s) (fun a s -> a + s) input 10.0
        let expected = [| 11.0; 12.0; 13.0; 14.0; 15.0; 16.0; 17.0; 18.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mapScalar multiplies each element by scalar (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v * s) (fun a s -> a * s) input 3.0
        let expected = [| 3.0; 6.0; 9.0; 12.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mapScalar subtracts scalar from each element (float)`` () =
        let input = [| 10.0; 20.0; 30.0; 40.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v - s) (fun a s -> a - s) input 5.0
        let expected = [| 5.0; 15.0; 25.0; 35.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mapScalar divides each element by scalar (float)`` () =
        let input = [| 10.0; 20.0; 30.0; 40.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v / s) (fun a s -> a / s) input 10.0
        let expected = [| 1.0; 2.0; 3.0; 4.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mapScalar with zero scalar multiplication (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v * s) (fun a s -> a * s) input 0.0
        let expected = [| 0.0; 0.0; 0.0; 0.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mapScalar with single element (float)`` () =
        let input = [| 5.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v * s) (fun a s -> a * s) input 7.0
        let expected = [| 35.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mapScalar with empty array (float)`` () =
        let input = [||] : float[]
        let result = SIMDUtils.mapScalar (fun v s -> v * s) (fun a s -> a * s) input 5.0
        Assert.Equal(0, result.Length)

    [<Fact>]
    let ``mapScalar with integers (int)`` () =
        let input = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let result = SIMDUtils.mapScalar (fun v s -> v * s) (fun a s -> a * s) input 2
        let expected = [| 2; 4; 6; 8; 10; 12; 14; 16 |]
        intArrayEqual expected result

    [<Fact>]
    let ``mapScalar handles non-SIMD-aligned sizes (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = SIMDUtils.mapScalar (fun v s -> v + s) (fun a s -> a + s) input 100.0
        let expected = [| 101.0; 102.0; 103.0; 104.0; 105.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // mapFoldUnchecked Tests
    // =============================================

    [<Fact>]
    let ``mapFoldUnchecked squares and sums (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |].AsSpan()
        let result = SIMDUtils.mapFoldUnchecked(
            (fun v -> v * v),
            (fun x -> x * x),
            (fun a b -> a + b),
            (fun a b -> a + b),
            0.0,
            input)
        // 1^2 + 2^2 + 3^2 + 4^2 = 1 + 4 + 9 + 16 = 30
        floatEqual 30.0 result 1e-10

    [<Fact>]
    let ``mapFoldUnchecked doubles and sums (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0 |].AsSpan()
        let result = SIMDUtils.mapFoldUnchecked(
            (fun v -> v * 2.0),
            (fun x -> x * 2.0),
            (fun a b -> a + b),
            (fun a b -> a + b),
            0.0,
            input)
        // (1*2) + (2*2) + (3*2) + (4*2) + (5*2) = 2 + 4 + 6 + 8 + 10 = 30
        floatEqual 30.0 result 1e-10

    [<Fact>]
    let ``mapFoldUnchecked with empty span (float)`` () =
        let input = ReadOnlySpan<float>.Empty
        let result = SIMDUtils.mapFoldUnchecked(
            (fun v -> v * 2.0),
            (fun x -> x * 2.0),
            (fun a b -> a + b),
            (fun a b -> a + b),
            0.0,
            input)
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``mapFoldUnchecked with single element (float)`` () =
        let input = [| 5.0 |].AsSpan()
        let result = SIMDUtils.mapFoldUnchecked(
            (fun v -> v * 3.0),
            (fun x -> x * 3.0),
            (fun a b -> a + b),
            (fun a b -> a + b),
            0.0,
            input)
        floatEqual 15.0 result 1e-10

    [<Fact>]
    let ``mapFoldUnchecked with large array (float)`` () =
        let size = 1000
        let arr = Array.init size (fun _ -> 1.0)
        let input = ReadOnlySpan(arr)
        let result = SIMDUtils.mapFoldUnchecked(
            (fun v -> v),
            (fun x -> x),
            (fun a b -> a + b),
            (fun a b -> a + b),
            0.0,
            input)
        floatEqual (float size) result 1e-8

    // =============================================
    // mapRangeInPlace Tests
    // =============================================

    [<Fact>]
    let ``mapRangeInPlace modifies specified range (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |]
        SIMDUtils.mapRangeInPlace (fun v -> v * 2.0) (fun x -> x * 2.0) 2 4 input
        let expected = [| 1.0; 2.0; 6.0; 8.0; 10.0; 12.0; 7.0; 8.0 |]
        floatArrayClose expected input 1e-10

    [<Fact>]
    let ``mapRangeInPlace modifies entire array (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        SIMDUtils.mapRangeInPlace (fun v -> v + Numerics.Vector(10.0)) (fun x -> x + 10.0) 0 4 input
        let expected = [| 11.0; 12.0; 13.0; 14.0 |]
        floatArrayClose expected input 1e-10

    [<Fact>]
    let ``mapRangeInPlace modifies from middle to end (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        SIMDUtils.mapRangeInPlace (fun v -> v * 3.0) (fun x -> x * 3.0) 2 3 input
        let expected = [| 1.0; 2.0; 9.0; 12.0; 15.0 |]
        floatArrayClose expected input 1e-10

    [<Fact>]
    let ``mapRangeInPlace with count zero does nothing (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        let original = Array.copy input
        SIMDUtils.mapRangeInPlace (fun v -> v * 2.0) (fun x -> x * 2.0) 0 0 input
        floatArrayClose original input 1e-10

    [<Fact>]
    let ``mapRangeInPlace with single element range (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        SIMDUtils.mapRangeInPlace (fun v -> v * 10.0) (fun x -> x * 10.0) 2 1 input
        let expected = [| 1.0; 2.0; 30.0; 4.0 |]
        floatArrayClose expected input 1e-10

    [<Fact>]
    let ``mapRangeInPlace throws on invalid start index (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.mapRangeInPlace (fun v -> v) (fun x -> x) -1 2 input)

    [<Fact>]
    let ``mapRangeInPlace throws on invalid count (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.mapRangeInPlace (fun v -> v) (fun x -> x) 0 -1 input)

    [<Fact>]
    let ``mapRangeInPlace throws on out of bounds range (float)`` () =
        let input = [| 1.0; 2.0; 3.0; 4.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.mapRangeInPlace (fun v -> v) (fun x -> x) 2 5 input)

    // =============================================
    // map2RangeInPlace Tests
    // =============================================

    [<Fact>]
    let ``map2RangeInPlace adds ranges (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |]
        let src = [| 10.0; 20.0; 30.0; 40.0; 50.0; 60.0 |]
        SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 1 1 3 dst src
        let expected = [| 1.0; 22.0; 33.0; 44.0; 5.0; 6.0 |]
        floatArrayClose expected dst 1e-10

    [<Fact>]
    let ``map2RangeInPlace multiplies ranges (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        SIMDUtils.map2RangeInPlace (fun a b -> a * b) (fun a b -> a * b) 0 0 4 dst src
        let expected = [| 5.0; 12.0; 21.0; 32.0 |]
        floatArrayClose expected dst 1e-10

    [<Fact>]
    let ``map2RangeInPlace with different start indices (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let src = [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
        SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 2 0 2 dst src
        let expected = [| 1.0; 2.0; 13.0; 24.0; 5.0 |]
        floatArrayClose expected dst 1e-10

    [<Fact>]
    let ``map2RangeInPlace with count zero does nothing (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        let original = Array.copy dst
        SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 0 0 0 dst src
        floatArrayClose original dst 1e-10

    [<Fact>]
    let ``map2RangeInPlace with single element (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 10.0; 20.0; 30.0; 40.0 |]
        SIMDUtils.map2RangeInPlace (fun a b -> a * b) (fun a b -> a * b) 2 1 1 dst src
        let expected = [| 1.0; 2.0; 60.0; 4.0 |]
        floatArrayClose expected dst 1e-10

    [<Fact>]
    let ``map2RangeInPlace throws on negative dst start index (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) -1 0 2 dst src)

    [<Fact>]
    let ``map2RangeInPlace throws on negative src start index (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 0 -1 2 dst src)

    [<Fact>]
    let ``map2RangeInPlace throws on negative count (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 0 0 -1 dst src)

    [<Fact>]
    let ``map2RangeInPlace throws on dst out of bounds (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 2 0 5 dst src)

    [<Fact>]
    let ``map2RangeInPlace throws on src out of bounds (float)`` () =
        let dst = [| 1.0; 2.0; 3.0; 4.0 |]
        let src = [| 5.0; 6.0; 7.0; 8.0 |]
        throws<ArgumentException>(fun () ->
            SIMDUtils.map2RangeInPlace (fun a b -> a + b) (fun a b -> a + b) 0 2 5 dst src)
