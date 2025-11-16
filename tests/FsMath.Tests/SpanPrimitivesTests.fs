namespace FsMath.Tests

open System
open Xunit
open FsMath

module SpanPrimitivesTestWrappers =
    /// Non-inline wrapper functions to enable coverage tracking of SpanINumberPrimitives inline functions
    /// These wrappers call the inline functions and make them visible to coverage tools

    let mapIntoW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src: ReadOnlySpan<'T>, dst: Span<'T>) =
        SpanINumberPrimitives.mapInto(fv, f, src, dst)

    let mapIntoWithOffsetsW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src: ReadOnlySpan<'T>, dst: Span<'T>, srcOffset, dstOffset, length) =
        SpanINumberPrimitives.mapInto(fv, f, src, dst, srcOffset, dstOffset, length)

    let map2IntoW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src1: ReadOnlySpan<'T>, src2: ReadOnlySpan<'T>, dst: Span<'T>) =
        SpanINumberPrimitives.map2Into(fv, f, src1, src2, dst)

    let map2IntoWithOffsetsW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src1: ReadOnlySpan<'T>, src2: ReadOnlySpan<'T>, dst: Span<'T>, src1Offset, src2Offset, dstOffset, length) =
        SpanINumberPrimitives.map2Into(fv, f, src1, src2, dst, src1Offset, src2Offset, dstOffset, length)

    let mapScalarIntoW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src: ReadOnlySpan<'T>, dst: Span<'T>, scalar: 'T) =
        SpanINumberPrimitives.mapScalarInto(fv, f, src, dst, scalar)

    let mapScalarIntoWithOffsetsW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src: ReadOnlySpan<'T>, dst: Span<'T>, scalar: 'T, srcOffset, dstOffset, length) =
        SpanINumberPrimitives.mapScalarInto(fv, f, src, dst, scalar, srcOffset, dstOffset, length)

    let foldW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src: ReadOnlySpan<'T>, init: 'T) : 'T =
        SpanINumberPrimitives.fold(fv, f, src, init)

    let foldWithOffsetsW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, src: ReadOnlySpan<'T>, init: 'T, offset, length) : 'T =
        SpanINumberPrimitives.fold(fv, f, src, init, offset, length)

    let fold2W<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, x: ReadOnlySpan<'T>, y: ReadOnlySpan<'T>, init: 'T) : 'T =
        SpanINumberPrimitives.fold2(fv, f, x, y, init)

    let fold2WithOffsetsW<'T when 'T :> Numerics.INumber<'T> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>
        (fv, f, x: ReadOnlySpan<'T>, y: ReadOnlySpan<'T>, init: 'T, xOffset, yOffset, length) : 'T =
        SpanINumberPrimitives.fold2(fv, f, x, y, init, xOffset, yOffset, length)

module SpanPrimitivesMapTests =
    open SpanPrimitivesTestWrappers

    [<Fact>]
    let ``mapInto with float addition works correctly`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0 |])
        let dst = Span<float>(Array.zeroCreate 4)

        mapIntoW((fun v -> v + Numerics.Vector<float>(1.0)), (fun x -> x + 1.0), src, dst)

        Assert.Equal(2.0, dst.[0])
        Assert.Equal(3.0, dst.[1])
        Assert.Equal(4.0, dst.[2])
        Assert.Equal(5.0, dst.[3])

    [<Fact>]
    let ``mapInto with int multiplication works correctly`` () =
        let src = ReadOnlySpan<int>([| 2; 3; 4; 5 |])
        let dst = Span<int>(Array.zeroCreate 4)

        mapIntoW((fun v -> v * Numerics.Vector<int>(2)), (fun x -> x * 2), src, dst)

        Assert.Equal(4, dst.[0])
        Assert.Equal(6, dst.[1])
        Assert.Equal(8, dst.[2])
        Assert.Equal(10, dst.[3])

    [<Fact>]
    let ``mapInto with offset and length works correctly`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0; 5.0 |])
        let dst = Span<float>(Array.zeroCreate 5)

        mapIntoWithOffsetsW((fun v -> v * Numerics.Vector<float>(2.0)), (fun x -> x * 2.0), src, dst, 1, 1, 3)

        Assert.Equal(0.0, dst.[0])
        Assert.Equal(4.0, dst.[1])
        Assert.Equal(6.0, dst.[2])
        Assert.Equal(8.0, dst.[3])
        Assert.Equal(0.0, dst.[4])

    [<Fact>]
    let ``mapInto with large array uses SIMD path`` () =
        let size = 100
        let src = ReadOnlySpan<float>(Array.init size float)
        let dst = Span<float>(Array.zeroCreate size)

        mapIntoW((fun v -> v + Numerics.Vector<float>(1.0)), (fun x -> x + 1.0), src, dst)

        for i in 0 .. size - 1 do
            Assert.Equal(float i + 1.0, dst.[i])

    [<Fact>]
    let ``mapInto handles single element correctly`` () =
        let src = ReadOnlySpan<float>([| 5.0 |])
        let dst = Span<float>(Array.zeroCreate 1)

        mapIntoW((fun v -> v * Numerics.Vector<float>(2.0)), (fun x -> x * 2.0), src, dst)

        Assert.Equal(10.0, dst.[0])

    [<Fact>]
    let ``mapInto handles empty length`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0 |])
        let dst = Span<float>([| 99.0; 99.0 |])

        mapIntoWithOffsetsW((fun v -> v), (fun x -> x + 1.0), src, dst, 0, 0, 0)

        Assert.Equal(99.0, dst.[0])
        Assert.Equal(99.0, dst.[1])

module SpanPrimitivesMap2Tests =
    open SpanPrimitivesTestWrappers

    [<Fact>]
    let ``map2Into addition works correctly`` () =
        let src1 = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0 |])
        let src2 = ReadOnlySpan<float>([| 5.0; 6.0; 7.0; 8.0 |])
        let dst = Span<float>(Array.zeroCreate 4)

        map2IntoW((fun v1 v2 -> v1 + v2), (fun x y -> x + y), src1, src2, dst)

        Assert.Equal(6.0, dst.[0])
        Assert.Equal(8.0, dst.[1])
        Assert.Equal(10.0, dst.[2])
        Assert.Equal(12.0, dst.[3])

    [<Fact>]
    let ``map2Into multiplication works correctly`` () =
        let src1 = ReadOnlySpan<int>([| 2; 3; 4; 5 |])
        let src2 = ReadOnlySpan<int>([| 10; 10; 10; 10 |])
        let dst = Span<int>(Array.zeroCreate 4)

        map2IntoW((fun v1 v2 -> v1 * v2), (fun x y -> x * y), src1, src2, dst)

        Assert.Equal(20, dst.[0])
        Assert.Equal(30, dst.[1])
        Assert.Equal(40, dst.[2])
        Assert.Equal(50, dst.[3])

    [<Fact>]
    let ``map2Into with offset and length works correctly`` () =
        let src1 = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0; 5.0 |])
        let src2 = ReadOnlySpan<float>([| 10.0; 20.0; 30.0; 40.0; 50.0 |])
        let dst = Span<float>(Array.zeroCreate 5)

        map2IntoWithOffsetsW((fun v1 v2 -> v1 + v2), (fun x y -> x + y), src1, src2, dst, 1, 1, 1, 3)

        Assert.Equal(0.0, dst.[0])
        Assert.Equal(22.0, dst.[1])
        Assert.Equal(33.0, dst.[2])
        Assert.Equal(44.0, dst.[3])
        Assert.Equal(0.0, dst.[4])

    [<Fact>]
    let ``map2Into with large arrays uses SIMD path`` () =
        let size = 100
        let src1 = ReadOnlySpan<float>(Array.create size 2.0)
        let src2 = ReadOnlySpan<float>(Array.create size 3.0)
        let dst = Span<float>(Array.zeroCreate size)

        map2IntoW((fun v1 v2 -> v1 * v2), (fun x y -> x * y), src1, src2, dst)

        for i in 0 .. size - 1 do
            Assert.Equal(6.0, dst.[i])

    [<Fact>]
    let ``map2Into handles single element correctly`` () =
        let src1 = ReadOnlySpan<float>([| 3.0 |])
        let src2 = ReadOnlySpan<float>([| 7.0 |])
        let dst = Span<float>(Array.zeroCreate 1)

        map2IntoW((fun v1 v2 -> v1 + v2), (fun x y -> x + y), src1, src2, dst)

        Assert.Equal(10.0, dst.[0])

    [<Fact>]
    let ``map2Into handles empty length`` () =
        let src1 = ReadOnlySpan<float>([| 1.0; 2.0 |])
        let src2 = ReadOnlySpan<float>([| 3.0; 4.0 |])
        let dst = Span<float>([| 99.0; 99.0 |])

        map2IntoWithOffsetsW((fun v1 v2 -> v1 + v2), (fun x y -> x + y), src1, src2, dst, 0, 0, 0, 0)

        Assert.Equal(99.0, dst.[0])
        Assert.Equal(99.0, dst.[1])

module SpanPrimitivesMapScalarTests =
    open SpanPrimitivesTestWrappers

    [<Fact>]
    let ``mapScalarInto addition works correctly`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0 |])
        let dst = Span<float>(Array.zeroCreate 4)
        let scalar = 10.0

        mapScalarIntoW((fun v s -> v + s), (fun x s -> x + s), src, dst, scalar)

        Assert.Equal(11.0, dst.[0])
        Assert.Equal(12.0, dst.[1])
        Assert.Equal(13.0, dst.[2])
        Assert.Equal(14.0, dst.[3])

    [<Fact>]
    let ``mapScalarInto multiplication works correctly`` () =
        let src = ReadOnlySpan<int>([| 2; 3; 4; 5 |])
        let dst = Span<int>(Array.zeroCreate 4)
        let scalar = 3

        mapScalarIntoW((fun v s -> v * s), (fun x s -> x * s), src, dst, scalar)

        Assert.Equal(6, dst.[0])
        Assert.Equal(9, dst.[1])
        Assert.Equal(12, dst.[2])
        Assert.Equal(15, dst.[3])

    [<Fact>]
    let ``mapScalarInto with offset and length works correctly`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0; 5.0 |])
        let dst = Span<float>(Array.zeroCreate 5)
        let scalar = 100.0

        mapScalarIntoWithOffsetsW((fun v s -> v + s), (fun x s -> x + s), src, dst, scalar, 1, 1, 3)

        Assert.Equal(0.0, dst.[0])
        Assert.Equal(102.0, dst.[1])
        Assert.Equal(103.0, dst.[2])
        Assert.Equal(104.0, dst.[3])
        Assert.Equal(0.0, dst.[4])

    [<Fact>]
    let ``mapScalarInto with large array uses SIMD path`` () =
        let size = 100
        let src = ReadOnlySpan<float>(Array.init size float)
        let dst = Span<float>(Array.zeroCreate size)
        let scalar = 10.0

        mapScalarIntoW((fun v s -> v + s), (fun x s -> x + s), src, dst, scalar)

        for i in 0 .. size - 1 do
            Assert.Equal(float i + 10.0, dst.[i])

    [<Fact>]
    let ``mapScalarInto handles single element correctly`` () =
        let src = ReadOnlySpan<float>([| 8.0 |])
        let dst = Span<float>(Array.zeroCreate 1)

        mapScalarIntoW((fun v s -> v / s), (fun x s -> x / s), src, dst, 2.0)

        Assert.Equal(4.0, dst.[0])

    [<Fact>]
    let ``mapScalarInto handles empty length`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0 |])
        let dst = Span<float>([| 99.0; 99.0 |])
        let scalar = 10.0

        mapScalarIntoWithOffsetsW((fun v s -> v + s), (fun x s -> x + s), src, dst, scalar, 0, 0, 0)

        Assert.Equal(99.0, dst.[0])
        Assert.Equal(99.0, dst.[1])

module SpanPrimitivesFoldTests =
    open SpanPrimitivesTestWrappers

    [<Fact>]
    let ``fold sum works correctly`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0; 3.0; 4.0 |])

        let result = foldW((fun acc v -> acc + v), (fun acc x -> acc + x), src, 0.0)

        Assert.Equal(10.0, result)

    [<Fact>]
    let ``fold product works correctly`` () =
        let src = ReadOnlySpan<int>([| 2; 3; 4 |])

        let result = foldW((fun acc v -> acc * v), (fun acc x -> acc * x), src, 1)

        Assert.Equal(24, result)

    [<Fact>]
    let ``fold with offset and length works correctly`` () =
        let src = ReadOnlySpan<float>([| 10.0; 1.0; 2.0; 3.0; 10.0 |])

        let result = foldWithOffsetsW((fun acc v -> acc + v), (fun acc x -> acc + x), src, 0.0, 1, 3)

        Assert.Equal(6.0, result)

    [<Fact>]
    let ``fold with empty span returns init`` () =
        let src = ReadOnlySpan<float>([| 1.0; 2.0 |])

        let result = foldWithOffsetsW((fun acc v -> acc + v), (fun acc x -> acc + x), src, 100.0, 0, 0)

        Assert.Equal(100.0, result)

    [<Fact>]
    let ``fold with large array uses SIMD path`` () =
        let size = 100
        let src = ReadOnlySpan<float>(Array.create size 1.0)

        let result = foldW((fun acc v -> acc + v), (fun acc x -> acc + x), src, 0.0)

        Assert.Equal(100.0, result)

    [<Fact>]
    let ``fold handles single element correctly`` () =
        let src = ReadOnlySpan<float>([| 42.0 |])

        let result = foldW((fun acc v -> acc + v), (fun acc x -> acc + x), src, 0.0)

        Assert.Equal(42.0, result)

module SpanPrimitivesFold2Tests =
    open SpanPrimitivesTestWrappers

    [<Fact>]
    let ``fold2 dot product works correctly`` () =
        let x = ReadOnlySpan<float>([| 1.0; 2.0; 3.0 |])
        let y = ReadOnlySpan<float>([| 4.0; 5.0; 6.0 |])

        let result = fold2W((fun acc vx vy -> acc + vx * vy), (fun acc x y -> acc + x * y), x, y, 0.0)

        Assert.Equal(32.0, result) // 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32

    [<Fact>]
    let ``fold2 sum of products works correctly`` () =
        let x = ReadOnlySpan<int>([| 2; 3; 4 |])
        let y = ReadOnlySpan<int>([| 10; 20; 30 |])

        let result = fold2W((fun acc vx vy -> acc + vx * vy), (fun acc x y -> acc + x * y), x, y, 0)

        Assert.Equal(200, result) // 2*10 + 3*20 + 4*30 = 20 + 60 + 120 = 200

    [<Fact>]
    let ``fold2 with offset and length works correctly`` () =
        let x = ReadOnlySpan<float>([| 99.0; 1.0; 2.0; 3.0; 99.0 |])
        let y = ReadOnlySpan<float>([| 99.0; 10.0; 20.0; 30.0; 99.0 |])

        let result = fold2WithOffsetsW((fun acc vx vy -> acc + vx * vy), (fun acc x y -> acc + x * y), x, y, 0.0, 1, 1, 3)

        Assert.Equal(140.0, result) // 1*10 + 2*20 + 3*30 = 10 + 40 + 90 = 140

    [<Fact>]
    let ``fold2 with empty span returns init`` () =
        let x = ReadOnlySpan<float>([| 1.0; 2.0 |])
        let y = ReadOnlySpan<float>([| 3.0; 4.0 |])

        let result = fold2WithOffsetsW((fun acc vx vy -> acc + vx + vy), (fun acc x y -> acc + x + y), x, y, 100.0, 0, 0, 0)

        Assert.Equal(100.0, result)

    [<Fact>]
    let ``fold2 with large arrays uses SIMD path`` () =
        let size = 100
        let x = ReadOnlySpan<float>(Array.create size 2.0)
        let y = ReadOnlySpan<float>(Array.create size 3.0)

        let result = fold2W((fun acc vx vy -> acc + vx * vy), (fun acc x y -> acc + x * y), x, y, 0.0)

        Assert.Equal(600.0, result) // 100 * (2.0 * 3.0)

    [<Fact>]
    let ``fold2 handles single element correctly`` () =
        let x = ReadOnlySpan<float>([| 6.0 |])
        let y = ReadOnlySpan<float>([| 7.0 |])

        let result = fold2W((fun acc vx vy -> acc + vx * vy), (fun acc x y -> acc + x * y), x, y, 0.0)

        Assert.Equal(42.0, result)
