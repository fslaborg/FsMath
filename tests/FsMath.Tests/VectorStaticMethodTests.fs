namespace FsMath.Tests.Vector

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle

/// Comprehensive tests for Vector static methods
module VectorStaticMethodTests =

    // =============================================
    // Vector.zeroCreate Tests
    // =============================================

    [<Fact>]
    let ``zeroCreate creates vector of correct length (float)`` () =
        let v = Vector.zeroCreate<float> 5
        Assert.Equal(5, v.Length)

    [<Fact>]
    let ``zeroCreate initializes all elements to zero (float)`` () =
        let v = Vector.zeroCreate<float> 5
        let expected = [| 0.0; 0.0; 0.0; 0.0; 0.0 |]
        floatArrayClose expected v 1e-10

    [<Fact>]
    let ``zeroCreate initializes all elements to zero (int)`` () =
        let v = Vector.zeroCreate<int> 3
        let expected = [| 0; 0; 0 |]
        Assert.Equal<Vector<int>>(expected, v)

    [<Fact>]
    let ``zeroCreate with size 0 creates empty vector`` () =
        let v = Vector.zeroCreate<float> 0
        Assert.Equal(0, v.Length)

    [<Fact>]
    let ``zeroCreate with size 1 creates single-element zero vector`` () =
        let v = Vector.zeroCreate<float> 1
        Assert.Equal<Vector<float>>([| 0.0 |], v)

    // =============================================
    // Vector.multiply Tests (static method, not operator)
    // =============================================

    [<Fact>]
    let ``multiply: element-wise multiplication (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = Vector.multiply v1 v2
        let expected = [| 4.0; 10.0; 18.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiply: element-wise multiplication (int)`` () =
        let v1 = [| 2; 3; 4 |]
        let v2 = [| 5; 6; 7 |]
        let result = Vector.multiply v1 v2
        let expected = [| 10; 18; 28 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``multiply: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> Vector.multiply v1 v2 |> ignore)

    [<Fact>]
    let ``multiply: with zeros produces zero vector`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 0.0; 0.0; 0.0 |]
        let result = Vector.multiply v1 v2
        let expected = [| 0.0; 0.0; 0.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Vector.divide Tests (static method, not operator)
    // =============================================

    [<Fact>]
    let ``divide: element-wise division (float)`` () =
        let v1 = [| 10.0; 20.0; 30.0 |]
        let v2 = [| 2.0; 5.0; 6.0 |]
        let result = Vector.divide v1 v2
        let expected = [| 5.0; 4.0; 5.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``divide: element-wise division (int)`` () =
        let v1 = [| 20; 30; 40 |]
        let v2 = [| 2; 3; 4 |]
        let result = Vector.divide v1 v2
        let expected = [| 10; 10; 10 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``divide: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> Vector.divide v1 v2 |> ignore)

    // =============================================
    // Vector.sum Tests
    // =============================================

    [<Fact>]
    let ``sum: computes sum of all elements (float)`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = Vector.sum v
        floatEqual 10.0 result 1e-10

    [<Fact>]
    let ``sum: computes sum of all elements (int)`` () =
        let v = [| 1; 2; 3; 4; 5 |]
        let result = Vector.sum v
        Assert.Equal(15, result)

    [<Fact>]
    let ``sum: empty vector sums to zero (float)`` () =
        let v = Array.empty<float>
        let result = Vector.sum v
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``sum: empty vector sums to zero (int)`` () =
        let v = Array.empty<int>
        let result = Vector.sum v
        Assert.Equal(0, result)

    [<Fact>]
    let ``sum: single element vector`` () =
        let v = [| 5.0 |]
        let result = Vector.sum v
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``sum: negative values (float)`` () =
        let v = [| 1.0; -2.0; 3.0; -4.0 |]
        let result = Vector.sum v
        floatEqual -2.0 result 1e-10

    [<Fact>]
    let ``sum: large vector (float)`` () =
        let v = Array.init 1000 (fun i -> float i)
        let result = Vector.sum v
        // Sum from 0 to 999 = 999 * 1000 / 2 = 499500
        floatEqual 499500.0 result 1e-8

    // =============================================
    // Vector.min Tests
    // =============================================

    [<Fact>]
    let ``min: finds minimum value (float)`` () =
        let v = [| 3.0; 1.0; 4.0; 1.0; 5.0 |]
        let result = Vector.min v
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``min: finds minimum value (int)`` () =
        let v = [| 5; 2; 8; 1; 9 |]
        let result = Vector.min v
        Assert.Equal(1, result)

    [<Fact>]
    let ``min: single element vector`` () =
        let v = [| 42.0 |]
        let result = Vector.min v
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``min: all elements equal`` () =
        let v = [| 7.0; 7.0; 7.0; 7.0 |]
        let result = Vector.min v
        floatEqual 7.0 result 1e-10

    [<Fact>]
    let ``min: negative values`` () =
        let v = [| -1.0; -5.0; -3.0; -2.0 |]
        let result = Vector.min v
        floatEqual -5.0 result 1e-10

    [<Fact>]
    let ``min: mixed positive and negative`` () =
        let v = [| 10.0; -5.0; 3.0; -2.0; 0.0 |]
        let result = Vector.min v
        floatEqual -5.0 result 1e-10

    [<Fact>]
    let ``min: minimum at start`` () =
        let v = [| 1.0; 5.0; 3.0; 4.0 |]
        let result = Vector.min v
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``min: minimum at end`` () =
        let v = [| 5.0; 3.0; 4.0; 1.0 |]
        let result = Vector.min v
        floatEqual 1.0 result 1e-10

    // =============================================
    // Vector.max Tests
    // =============================================

    [<Fact>]
    let ``max: finds maximum value (float)`` () =
        let v = [| 3.0; 1.0; 4.0; 1.0; 5.0 |]
        let result = Vector.max v
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``max: finds maximum value (int)`` () =
        let v = [| 5; 2; 8; 1; 9 |]
        let result = Vector.max v
        Assert.Equal(9, result)

    [<Fact>]
    let ``max: single element vector`` () =
        let v = [| 42.0 |]
        let result = Vector.max v
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``max: all elements equal`` () =
        let v = [| 7.0; 7.0; 7.0; 7.0 |]
        let result = Vector.max v
        floatEqual 7.0 result 1e-10

    [<Fact>]
    let ``max: negative values`` () =
        let v = [| -1.0; -5.0; -3.0; -2.0 |]
        let result = Vector.max v
        floatEqual -1.0 result 1e-10

    [<Fact>]
    let ``max: mixed positive and negative`` () =
        let v = [| 10.0; -5.0; 3.0; -2.0; 0.0 |]
        let result = Vector.max v
        floatEqual 10.0 result 1e-10

    [<Fact>]
    let ``max: maximum at start`` () =
        let v = [| 9.0; 5.0; 3.0; 4.0 |]
        let result = Vector.max v
        floatEqual 9.0 result 1e-10

    [<Fact>]
    let ``max: maximum at end`` () =
        let v = [| 5.0; 3.0; 4.0; 9.0 |]
        let result = Vector.max v
        floatEqual 9.0 result 1e-10

    // =============================================
    // Vector.addScalar Tests (static method)
    // =============================================

    [<Fact>]
    let ``addScalar: adds scalar to each element (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.addScalar 5.0 v
        let expected = [| 6.0; 7.0; 8.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``addScalar: adds scalar to each element (int)`` () =
        let v = [| 1; 2; 3 |]
        let result = Vector.addScalar 10 v
        let expected = [| 11; 12; 13 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``addScalar: zero scalar does not change vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.addScalar 0.0 v
        floatArrayClose v result 1e-10

    [<Fact>]
    let ``addScalar: negative scalar`` () =
        let v = [| 5.0; 6.0; 7.0 |]
        let result = Vector.addScalar -3.0 v
        let expected = [| 2.0; 3.0; 4.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Vector.subtractScalar Tests (static method)
    // =============================================

    [<Fact>]
    let ``subtractScalar: subtracts scalar from each element (float)`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = Vector.subtractScalar 5.0 v
        let expected = [| 5.0; 15.0; 25.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``subtractScalar: subtracts scalar from each element (int)`` () =
        let v = [| 10; 20; 30 |]
        let result = Vector.subtractScalar 5 v
        let expected = [| 5; 15; 25 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``subtractScalar: zero scalar does not change vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.subtractScalar 0.0 v
        floatArrayClose v result 1e-10

    [<Fact>]
    let ``subtractScalar: negative scalar`` () =
        let v = [| 5.0; 6.0; 7.0 |]
        let result = Vector.subtractScalar -3.0 v
        let expected = [| 8.0; 9.0; 10.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Vector.multiplyScalar Tests (static method)
    // =============================================

    [<Fact>]
    let ``multiplyScalar: multiplies each element by scalar (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.multiplyScalar 3.0 v
        let expected = [| 3.0; 6.0; 9.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyScalar: multiplies each element by scalar (int)`` () =
        let v = [| 2; 3; 4 |]
        let result = Vector.multiplyScalar 5 v
        let expected = [| 10; 15; 20 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``multiplyScalar: one scalar does not change vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.multiplyScalar 1.0 v
        floatArrayClose v result 1e-10

    [<Fact>]
    let ``multiplyScalar: zero scalar produces zero vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.multiplyScalar 0.0 v
        let expected = [| 0.0; 0.0; 0.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``multiplyScalar: negative scalar`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.multiplyScalar -2.0 v
        let expected = [| -2.0; -4.0; -6.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Vector.divideScalar Tests (static method)
    // =============================================

    [<Fact>]
    let ``divideScalar: divides each element by scalar (float)`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = Vector.divideScalar 10.0 v
        let expected = [| 1.0; 2.0; 3.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``divideScalar: divides each element by scalar (int)`` () =
        let v = [| 10; 20; 30 |]
        let result = Vector.divideScalar 5 v
        let expected = [| 2; 4; 6 |]
        Assert.Equal<Vector<int>>(expected, result)

    [<Fact>]
    let ``divideScalar: one scalar does not change vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.divideScalar 1.0 v
        floatArrayClose v result 1e-10

    [<Fact>]
    let ``divideScalar: negative scalar`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = Vector.divideScalar -10.0 v
        let expected = [| -1.0; -2.0; -3.0 |]
        floatArrayClose expected result 1e-10

    // =============================================
    // Edge Cases and Stress Tests
    // =============================================

    [<Fact>]
    let ``norm: zero vector has zero norm`` () =
        let v = [| 0.0; 0.0; 0.0 |]
        let result = Vector.norm v
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``norm: unit vectors have norm 1`` () =
        let v1 = [| 1.0; 0.0; 0.0 |]
        let result1 = Vector.norm v1
        floatEqual 1.0 result1 1e-10

        let v2 = [| 0.0; 1.0; 0.0 |]
        let result2 = Vector.norm v2
        floatEqual 1.0 result2 1e-10

    [<Fact>]
    let ``mean: single element vector returns that element`` () =
        let v = [| 7.0 |]
        let result = Vector.mean v
        floatEqual 7.0 result 1e-10

    [<Fact>]
    let ``product: vector with a zero returns zero`` () =
        let v = [| 2.0; 3.0; 0.0; 4.0 |]
        let result = Vector.product v
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``product: single element vector`` () =
        let v = [| 5.0 |]
        let result = Vector.product v
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``product: empty vector returns identity (1)`` () =
        let v = Array.empty<float>
        let result = Vector.product v
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``dot: zero-length vectors`` () =
        let v1 = Array.empty<float>
        let v2 = Array.empty<float>
        let result = Vector.dot v1 v2
        floatEqual 0.0 result 1e-10
