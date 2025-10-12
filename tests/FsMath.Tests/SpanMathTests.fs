namespace FsMath.Tests.SpanMath

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers

module SpanMathDotTests =

    [<Fact>]
    let ``dot: basic float vectors`` () =
        let x = ReadOnlySpan([| 1.0; 2.0; 3.0 |])
        let y = ReadOnlySpan([| 4.0; 5.0; 6.0 |])
        let result = SpanMath.dot(x, y)
        // 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        floatEqual 32.0 result 1e-10

    [<Fact>]
    let ``dot: basic int vectors`` () =
        let x = ReadOnlySpan([| 1; 2; 3 |])
        let y = ReadOnlySpan([| 4; 5; 6 |])
        let result = SpanMath.dot(x, y)
        intEqual 32 result

    [<Fact>]
    let ``dot: empty vectors`` () =
        let x = ReadOnlySpan([||]: float[])
        let y = ReadOnlySpan([||]: float[])
        let result = SpanMath.dot(x, y)
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``dot: single element vectors`` () =
        let x = ReadOnlySpan([| 5.0 |])
        let y = ReadOnlySpan([| 7.0 |])
        let result = SpanMath.dot(x, y)
        floatEqual 35.0 result 1e-10

    [<Fact>]
    let ``dot: throws on length mismatch`` () =
        let xArr = [| 1.0; 2.0 |]
        let yArr = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.dot(x, y) |> ignore)

    [<Fact>]
    let ``dot: large vector`` () =
        let size = 1000
        let x = ReadOnlySpan(Array.init size (fun i -> float i))
        let y = ReadOnlySpan(Array.init size (fun i -> 1.0))
        let result = SpanMath.dot(x, y)
        // sum of 0 to 999 = 999 * 1000 / 2 = 499500
        floatEqual 499500.0 result 1e-5

    [<Fact>]
    let ``dot: with offsets`` () =
        let x = ReadOnlySpan([| 1.0; 2.0; 3.0; 4.0; 5.0 |])
        let y = ReadOnlySpan([| 6.0; 7.0; 8.0; 9.0; 10.0 |])
        let result = SpanMath.dot(x, y, 1, 1, 3)
        // 2*7 + 3*8 + 4*9 = 14 + 24 + 36 = 74
        floatEqual 74.0 result 1e-10

    [<Fact>]
    let ``dot: throws on invalid offset`` () =
        let xArr = [| 1.0; 2.0; 3.0 |]
        let yArr = [| 4.0; 5.0; 6.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.dot(x, y, -1, 0, 2) |> ignore)

    [<Fact>]
    let ``dot: throws on invalid length`` () =
        let xArr = [| 1.0; 2.0; 3.0 |]
        let yArr = [| 4.0; 5.0; 6.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.dot(x, y, 0, 0, 10) |> ignore)


module SpanMathArithmeticTests =

    [<Fact>]
    let ``add: basic float vectors`` () =
        let x = ReadOnlySpan([| 1.0; 2.0; 3.0 |])
        let y = ReadOnlySpan([| 4.0; 5.0; 6.0 |])
        let result = SpanMath.add(x, y)
        floatArrayClose [| 5.0; 7.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``add: basic int vectors`` () =
        let x = ReadOnlySpan([| 1; 2; 3 |])
        let y = ReadOnlySpan([| 4; 5; 6 |])
        let result = SpanMath.add(x, y)
        intArrayEqual [| 5; 7; 9 |] result

    [<Fact>]
    let ``add: throws on dimension mismatch`` () =
        let xArr = [| 1.0; 2.0 |]
        let yArr = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.add(x, y) |> ignore)

    [<Fact>]
    let ``addScalar: basic float vector`` () =
        let x = ReadOnlySpan([| 1.0; 2.0; 3.0 |])
        let result = SpanMath.addScalar(x, 10.0)
        floatArrayClose [| 11.0; 12.0; 13.0 |] result 1e-10

    [<Fact>]
    let ``addScalar: basic int vector`` () =
        let x = ReadOnlySpan([| 1; 2; 3 |])
        let result = SpanMath.addScalar(x, 10)
        intArrayEqual [| 11; 12; 13 |] result

    [<Fact>]
    let ``subtract: basic float vectors`` () =
        let x = ReadOnlySpan([| 5.0; 7.0; 9.0 |])
        let y = ReadOnlySpan([| 1.0; 2.0; 3.0 |])
        let result = SpanMath.subtract(x, y)
        floatArrayClose [| 4.0; 5.0; 6.0 |] result 1e-10

    [<Fact>]
    let ``subtract: basic int vectors`` () =
        let x = ReadOnlySpan([| 5; 7; 9 |])
        let y = ReadOnlySpan([| 1; 2; 3 |])
        let result = SpanMath.subtract(x, y)
        intArrayEqual [| 4; 5; 6 |] result

    [<Fact>]
    let ``subtract: throws on dimension mismatch`` () =
        let xArr = [| 1.0; 2.0 |]
        let yArr = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.subtract(x, y) |> ignore)

    [<Fact>]
    let ``subtractScalar: basic float vector`` () =
        let x = ReadOnlySpan([| 11.0; 12.0; 13.0 |])
        let result = SpanMath.subtractScalar(x, 10.0)
        floatArrayClose [| 1.0; 2.0; 3.0 |] result 1e-10

    [<Fact>]
    let ``subtractScalar: basic int vector`` () =
        let x = ReadOnlySpan([| 11; 12; 13 |])
        let result = SpanMath.subtractScalar(x, 10)
        intArrayEqual [| 1; 2; 3 |] result

    [<Fact>]
    let ``multiply: basic float vectors`` () =
        let x = ReadOnlySpan([| 2.0; 3.0; 4.0 |])
        let y = ReadOnlySpan([| 5.0; 6.0; 7.0 |])
        let result = SpanMath.multiply(x, y)
        floatArrayClose [| 10.0; 18.0; 28.0 |] result 1e-10

    [<Fact>]
    let ``multiply: basic int vectors`` () =
        let x = ReadOnlySpan([| 2; 3; 4 |])
        let y = ReadOnlySpan([| 5; 6; 7 |])
        let result = SpanMath.multiply(x, y)
        intArrayEqual [| 10; 18; 28 |] result

    [<Fact>]
    let ``multiply: throws on dimension mismatch`` () =
        let xArr = [| 1.0; 2.0 |]
        let yArr = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.multiply(x, y) |> ignore)

    [<Fact>]
    let ``multiplyScalar: basic float vector`` () =
        let x = ReadOnlySpan([| 2.0; 3.0; 4.0 |])
        let result = SpanMath.multiplyScalar(x, 10.0)
        floatArrayClose [| 20.0; 30.0; 40.0 |] result 1e-10

    [<Fact>]
    let ``multiplyScalar: basic int vector`` () =
        let x = ReadOnlySpan([| 2; 3; 4 |])
        let result = SpanMath.multiplyScalar(x, 10)
        intArrayEqual [| 20; 30; 40 |] result

    [<Fact>]
    let ``divide: basic float vectors`` () =
        let x = ReadOnlySpan([| 10.0; 18.0; 28.0 |])
        let y = ReadOnlySpan([| 2.0; 3.0; 4.0 |])
        let result = SpanMath.divide(x, y)
        floatArrayClose [| 5.0; 6.0; 7.0 |] result 1e-10

    [<Fact>]
    let ``divide: basic int vectors`` () =
        let x = ReadOnlySpan([| 10; 18; 28 |])
        let y = ReadOnlySpan([| 2; 3; 4 |])
        let result = SpanMath.divide(x, y)
        intArrayEqual [| 5; 6; 7 |] result

    [<Fact>]
    let ``divide: throws on dimension mismatch`` () =
        let xArr = [| 1.0; 2.0 |]
        let yArr = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            let y = ReadOnlySpan(yArr)
            SpanMath.divide(x, y) |> ignore)

    [<Fact>]
    let ``divideScalar: basic float vector`` () =
        let x = ReadOnlySpan([| 20.0; 30.0; 40.0 |])
        let result = SpanMath.divideScalar(x, 10.0)
        floatArrayClose [| 2.0; 3.0; 4.0 |] result 1e-10

    [<Fact>]
    let ``divideScalar: basic int vector`` () =
        let x = ReadOnlySpan([| 20; 30; 40 |])
        let result = SpanMath.divideScalar(x, 10)
        intArrayEqual [| 2; 3; 4 |] result


module SpanMathAggregateTests =

    [<Fact>]
    let ``sum: basic float vector`` () =
        let x = ReadOnlySpan([| 1.0; 2.0; 3.0; 4.0 |])
        let result = SpanMath.sum(x)
        floatEqual 10.0 result 1e-10

    [<Fact>]
    let ``sum: basic int vector`` () =
        let x = ReadOnlySpan([| 1; 2; 3; 4 |])
        let result = SpanMath.sum(x)
        intEqual 10 result

    [<Fact>]
    let ``sum: empty vector`` () =
        let x = ReadOnlySpan([||]: float[])
        let result = SpanMath.sum(x)
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``sum: single element`` () =
        let x = ReadOnlySpan([| 42.0 |])
        let result = SpanMath.sum(x)
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``sum: negative values`` () =
        let x = ReadOnlySpan([| -1.0; -2.0; -3.0 |])
        let result = SpanMath.sum(x)
        floatEqual -6.0 result 1e-10

    [<Fact>]
    let ``product: basic float vector`` () =
        let x = ReadOnlySpan([| 2.0; 3.0; 4.0 |])
        let result = SpanMath.product(x)
        floatEqual 24.0 result 1e-10

    [<Fact>]
    let ``product: basic int vector`` () =
        let x = ReadOnlySpan([| 2; 3; 4 |])
        let result = SpanMath.product(x)
        intEqual 24 result

    [<Fact>]
    let ``product: empty vector`` () =
        let x = ReadOnlySpan([||]: float[])
        let result = SpanMath.product(x)
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``product: contains zero`` () =
        let x = ReadOnlySpan([| 2.0; 0.0; 4.0 |])
        let result = SpanMath.product(x)
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``mean: basic float vector`` () =
        let x = ReadOnlySpan([| 2.0; 4.0; 6.0; 8.0 |])
        let result = SpanMath.mean(x)
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``mean: throws on empty vector`` () =
        let xArr = [||]: float[]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            SpanMath.mean(x) |> ignore)

    [<Fact>]
    let ``mean: single element`` () =
        let x = ReadOnlySpan([| 42.0 |])
        let result = SpanMath.mean(x)
        floatEqual 42.0 result 1e-10


module SpanMathNormTests =

    [<Fact>]
    let ``norm: 3-4-5 triangle`` () =
        let x = ReadOnlySpan([| 3.0; 4.0 |])
        let result = SpanMath.norm(x)
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``norm: unit vector`` () =
        let x = ReadOnlySpan([| 1.0; 0.0; 0.0 |])
        let result = SpanMath.norm(x)
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``norm: zero vector`` () =
        let x = ReadOnlySpan([| 0.0; 0.0; 0.0 |])
        let result = SpanMath.norm(x)
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``norm: negative values`` () =
        let x = ReadOnlySpan([| -3.0; -4.0 |])
        let result = SpanMath.norm(x)
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``norm: single element`` () =
        let x = ReadOnlySpan([| 7.0 |])
        let result = SpanMath.norm(x)
        floatEqual 7.0 result 1e-10


module SpanMathMinMaxTests =

    [<Fact>]
    let ``min: basic float vector`` () =
        let x = ReadOnlySpan([| 3.0; 1.0; 4.0; 1.0; 5.0 |])
        let result = SpanMath.min(x)
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``min: basic int vector`` () =
        let x = ReadOnlySpan([| 3; 1; 4; 1; 5 |])
        let result = SpanMath.min(x)
        intEqual 1 result

    [<Fact>]
    let ``min: single element`` () =
        let x = ReadOnlySpan([| 42.0 |])
        let result = SpanMath.min(x)
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``min: negative values`` () =
        let x = ReadOnlySpan([| -1.0; -5.0; -2.0 |])
        let result = SpanMath.min(x)
        floatEqual -5.0 result 1e-10

    [<Fact>]
    let ``min: throws on empty vector`` () =
        let xArr = [||]: float[]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            SpanMath.min(x) |> ignore)

    [<Fact>]
    let ``max: basic float vector`` () =
        let x = ReadOnlySpan([| 3.0; 1.0; 4.0; 1.0; 5.0 |])
        let result = SpanMath.max(x)
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``max: basic int vector`` () =
        let x = ReadOnlySpan([| 3; 1; 4; 1; 5 |])
        let result = SpanMath.max(x)
        intEqual 5 result

    [<Fact>]
    let ``max: single element`` () =
        let x = ReadOnlySpan([| 42.0 |])
        let result = SpanMath.max(x)
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``max: negative values`` () =
        let x = ReadOnlySpan([| -1.0; -5.0; -2.0 |])
        let result = SpanMath.max(x)
        floatEqual -1.0 result 1e-10

    [<Fact>]
    let ``max: throws on empty vector`` () =
        let xArr = [||]: float[]
        throws<ArgumentException>(fun () ->
            let x = ReadOnlySpan(xArr)
            SpanMath.max(x) |> ignore)


module SpanMathOuterProductTests =

    [<Fact>]
    let ``outerProduct: basic 3x2 example (float)`` () =
        let u = ReadOnlySpan([| 1.0; 2.0; 3.0 |])
        let v = ReadOnlySpan([| 4.0; 5.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(3, rows)
        Assert.Equal(2, cols)
        Assert.Equal(6, data.Length)

        // Expected result:
        // [1*4  1*5]   [4  5]
        // [2*4  2*5] = [8  10]
        // [3*4  3*5]   [12 15]
        let expected = [| 4.0; 5.0; 8.0; 10.0; 12.0; 15.0 |]
        floatArrayClose expected data 1e-10

    [<Fact>]
    let ``outerProduct: basic 2x3 example (float)`` () =
        let u = ReadOnlySpan([| 2.0; 3.0 |])
        let v = ReadOnlySpan([| 1.0; 0.0; -1.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(2, rows)
        Assert.Equal(3, cols)

        // Expected result:
        // [2*1  2*0  2*(-1)]   [2  0  -2]
        // [3*1  3*0  3*(-1)] = [3  0  -3]
        let expected = [| 2.0; 0.0; -2.0; 3.0; 0.0; -3.0 |]
        floatArrayClose expected data 1e-10

    [<Fact>]
    let ``outerProduct: basic example (int)`` () =
        let u = ReadOnlySpan([| 1; 2; 3 |])
        let v = ReadOnlySpan([| 4; 5 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(3, rows)
        Assert.Equal(2, cols)

        let expected = [| 4; 5; 8; 10; 12; 15 |]
        intArrayEqual expected data

    [<Fact>]
    let ``outerProduct: single element vectors`` () =
        let u = ReadOnlySpan([| 7.0 |])
        let v = ReadOnlySpan([| 3.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(1, rows)
        Assert.Equal(1, cols)

        let expected = [| 21.0 |]
        floatArrayClose expected data 1e-10

    [<Fact>]
    let ``outerProduct: with zero elements`` () =
        let u = ReadOnlySpan([| 1.0; 0.0; 2.0 |])
        let v = ReadOnlySpan([| 3.0; 0.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(3, rows)
        Assert.Equal(2, cols)

        // Expected result:
        // [1*3  1*0]   [3  0]
        // [0*3  0*0] = [0  0]
        // [2*3  2*0]   [6  0]
        let expected = [| 3.0; 0.0; 0.0; 0.0; 6.0; 0.0 |]
        floatArrayClose expected data 1e-10

    [<Fact>]
    let ``outerProduct: with negative values`` () =
        let u = ReadOnlySpan([| 1.0; -2.0 |])
        let v = ReadOnlySpan([| -3.0; 4.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(2, rows)
        Assert.Equal(2, cols)

        // Expected result:
        // [1*(-3)   1*4]     [-3   4]
        // [(-2)*(-3) (-2)*4] = [6  -8]
        let expected = [| -3.0; 4.0; 6.0; -8.0 |]
        floatArrayClose expected data 1e-10

    [<Fact>]
    let ``outerProduct: larger vectors to test SIMD path`` () =
        let size1 = 20
        let size2 = 30
        let u = ReadOnlySpan(Array.init size1 (fun i -> float (i + 1)))
        let v = ReadOnlySpan(Array.init size2 (fun j -> float (j + 1)))
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(size1, rows)
        Assert.Equal(size2, cols)
        Assert.Equal(size1 * size2, data.Length)

        // Verify a few specific elements
        // data[0] should be u[0] * v[0] = 1 * 1 = 1
        floatEqual 1.0 data.[0] 1e-10

        // data[size2] should be u[1] * v[0] = 2 * 1 = 2
        floatEqual 2.0 data.[size2] 1e-10

        // Last element data[size1*size2-1] should be u[size1-1] * v[size2-1] = 20 * 30 = 600
        floatEqual 600.0 data.[size1 * size2 - 1] 1e-10

        // Middle element: data[10*size2 + 15] should be u[10] * v[15] = 11 * 16 = 176
        floatEqual 176.0 data.[10 * size2 + 15] 1e-10

    [<Fact>]
    let ``outerProduct: mathematical property - u ⊗ v creates rank-1 matrix`` () =
        let u = ReadOnlySpan([| 1.0; 2.0 |])
        let v = ReadOnlySpan([| 3.0; 4.0; 5.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        // All columns should be proportional to u
        // Column 0: [1*3; 2*3] = [3; 6] = 3 * u
        // Column 1: [1*4; 2*4] = [4; 8] = 4 * u
        // Column 2: [1*5; 2*5] = [5; 10] = 5 * u

        for j = 0 to cols - 1 do
            let ratio = v.[j]
            for i = 0 to rows - 1 do
                let expected = u.[i] * ratio
                floatEqual expected data.[i * cols + j] 1e-10

    [<Fact>]
    let ``outerProduct: identity-like vectors`` () =
        let u = ReadOnlySpan([| 1.0; 0.0; 0.0 |])
        let v = ReadOnlySpan([| 0.0; 1.0; 0.0 |])
        let (rows, cols, data) = SpanMath.outerProduct(u, v)

        Assert.Equal(3, rows)
        Assert.Equal(3, cols)

        // Expected result:
        // [1*0  1*1  1*0]   [0  1  0]
        // [0*0  0*1  0*0] = [0  0  0]
        // [0*0  0*1  0*0]   [0  0  0]
        let expected = [| 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 |]
        floatArrayClose expected data 1e-10

    [<Fact>]
    let ``outerProduct: commutative property size check`` () =
        let u = ReadOnlySpan([| 1.0; 2.0; 3.0 |])
        let v = ReadOnlySpan([| 4.0; 5.0 |])

        let (rows1, cols1, data1) = SpanMath.outerProduct(u, v)
        let (rows2, cols2, data2) = SpanMath.outerProduct(v, u)

        // u ⊗ v should be 3x2, v ⊗ u should be 2x3
        Assert.Equal(3, rows1)
        Assert.Equal(2, cols1)
        Assert.Equal(2, rows2)
        Assert.Equal(3, cols2)

        // Both should have same total elements
        Assert.Equal(data1.Length, data2.Length)

        // data1 is row-major 3x2, data2 is row-major 2x3
        // They should be transposes of each other
        for i = 0 to rows1 - 1 do
            for j = 0 to cols1 - 1 do
                floatEqual data1.[i * cols1 + j] data2.[j * cols2 + i] 1e-10
