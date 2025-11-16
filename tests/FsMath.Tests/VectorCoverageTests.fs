namespace FsMath.Tests.Vector

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FSharp.Linq.RuntimeHelpers

/// <summary>
/// Quotation-based tests for Vector static methods (inline functions).
/// These tests use F# quotation evaluation to ensure that inline functions
/// are actually executed during test coverage measurement.
///
/// Background: F# inline functions are not tracked by coverage tools because
/// they are inlined at compile time. Using quotations forces the code to be
/// evaluated dynamically, which allows coverage tools to track execution.
///
/// See: https://github.com/fslaborg/FsMath/discussions/5
/// </summary>
module VectorCoverageTests =

    /// Helper to evaluate quotations that return a value
    let inline evalQ<'T> (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        LeafExpressionConverter.EvaluateQuotation expr :?> 'T

    // ========================================
    // Quotation tests for zeroCreate
    // ========================================

    [<Fact>]
    let ``zeroCreate_Q: creates vector of zeros`` () =
        let result = evalQ <@ Vector.zeroCreate<float> 5 @>
        floatArrayClose [| 0.0; 0.0; 0.0; 0.0; 0.0 |] result 1e-10

    [<Fact>]
    let ``zeroCreate_Q: creates empty vector with size 0`` () =
        let result = evalQ <@ Vector.zeroCreate<float> 0 @>
        Assert.Empty(result)

    [<Fact>]
    let ``zeroCreate_Q: works with int type`` () =
        let result = evalQ <@ Vector.zeroCreate<int> 3 @>
        Assert.Equal<Vector<int>>([| 0; 0; 0 |], result)

    // ========================================
    // Quotation tests for add
    // ========================================

    [<Fact>]
    let ``add_Q: adds two vectors element-wise`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ Vector.add v1 v2 @>
        floatArrayClose [| 5.0; 7.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``add_Q: works with empty vectors`` () =
        let v1 = Array.empty<float>
        let v2 = Array.empty<float>
        let result = evalQ <@ Vector.add v1 v2 @>
        Assert.Empty(result)

    [<Fact>]
    let ``add_Q: works with single element`` () =
        let v1 = [| 5.0 |]
        let v2 = [| 3.0 |]
        let result = evalQ <@ Vector.add v1 v2 @>
        floatArrayClose [| 8.0 |] result 1e-10

    [<Fact>]
    let ``add_Q: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.add v1 v2 @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for subtract
    // ========================================

    [<Fact>]
    let ``subtract_Q: subtracts two vectors element-wise`` () =
        let v1 = [| 5.5; 5.0; 5.75 |]
        let v2 = [| 1.5; 2.0; 3.25 |]
        let result = evalQ <@ Vector.subtract v1 v2 @>
        floatArrayClose [| 4.0; 3.0; 2.5 |] result 1e-10

    [<Fact>]
    let ``subtract_Q: works with negative values`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ Vector.subtract v1 v2 @>
        floatArrayClose [| -3.0; -3.0; -3.0 |] result 1e-10

    [<Fact>]
    let ``subtract_Q: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.subtract v1 v2 @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for multiply
    // ========================================

    [<Fact>]
    let ``multiply_Q: multiplies two vectors element-wise`` () =
        let v1 = [| 2.0; 3.0; 4.0 |]
        let v2 = [| 5.0; 6.0; 7.0 |]
        let result = evalQ <@ Vector.multiply v1 v2 @>
        floatArrayClose [| 10.0; 18.0; 28.0 |] result 1e-10

    [<Fact>]
    let ``multiply_Q: handles zeros`` () =
        let v1 = [| 0.0; 5.0; 0.0 |]
        let v2 = [| 10.0; 2.0; 15.0 |]
        let result = evalQ <@ Vector.multiply v1 v2 @>
        floatArrayClose [| 0.0; 10.0; 0.0 |] result 1e-10

    [<Fact>]
    let ``multiply_Q: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.multiply v1 v2 @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for divide
    // ========================================

    [<Fact>]
    let ``divide_Q: divides two vectors element-wise`` () =
        let v1 = [| 10.0; 20.0; 30.0 |]
        let v2 = [| 2.0; 5.0; 5.0 |]
        let result = evalQ <@ Vector.divide v1 v2 @>
        floatArrayClose [| 5.0; 4.0; 6.0 |] result 1e-10

    [<Fact>]
    let ``divide_Q: handles fractional results`` () =
        let v1 = [| 1.0; 1.0; 1.0 |]
        let v2 = [| 3.0; 4.0; 5.0 |]
        let result = evalQ <@ Vector.divide v1 v2 @>
        floatArrayClose [| 0.333333; 0.25; 0.2 |] result 1e-5

    [<Fact>]
    let ``divide_Q: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.divide v1 v2 @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for addScalar
    // ========================================

    [<Fact>]
    let ``addScalar_Q: adds scalar to each element`` () =
        let v = [| 2.5; 4.0; 6.0 |]
        let result = evalQ <@ Vector.addScalar 2.0 v @>
        floatArrayClose [| 4.5; 6.0; 8.0 |] result 1e-10

    [<Fact>]
    let ``addScalar_Q: works with negative scalar`` () =
        let v = [| 5.0; 10.0; 15.0 |]
        let result = evalQ <@ Vector.addScalar -2.0 v @>
        floatArrayClose [| 3.0; 8.0; 13.0 |] result 1e-10

    [<Fact>]
    let ``addScalar_Q: works with empty vector`` () =
        let v = Array.empty<float>
        let result = evalQ <@ Vector.addScalar 5.0 v @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for subtractScalar
    // ========================================

    [<Fact>]
    let ``subtractScalar_Q: subtracts scalar from each element`` () =
        let v = [| 2.5; 4.0; 6.0 |]
        let result = evalQ <@ Vector.subtractScalar 0.5 v @>
        floatArrayClose [| 2.0; 3.5; 5.5 |] result 1e-10

    [<Fact>]
    let ``subtractScalar_Q: works with negative scalar`` () =
        let v = [| 5.0; 10.0; 15.0 |]
        let result = evalQ <@ Vector.subtractScalar -2.0 v @>
        floatArrayClose [| 7.0; 12.0; 17.0 |] result 1e-10

    // ========================================
    // Quotation tests for multiplyScalar
    // ========================================

    [<Fact>]
    let ``multiplyScalar_Q: multiplies each element by scalar`` () =
        let v = [| 2.5; 4.0; 6.0 |]
        let result = evalQ <@ Vector.multiplyScalar 1.5 v @>
        floatArrayClose [| 3.75; 6.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``multiplyScalar_Q: works with zero scalar`` () =
        let v = [| 5.0; 10.0; 15.0 |]
        let result = evalQ <@ Vector.multiplyScalar 0.0 v @>
        floatArrayClose [| 0.0; 0.0; 0.0 |] result 1e-10

    [<Fact>]
    let ``multiplyScalar_Q: works with negative scalar`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.multiplyScalar -2.0 v @>
        floatArrayClose [| -2.0; -4.0; -6.0 |] result 1e-10

    // ========================================
    // Quotation tests for divideScalar
    // ========================================

    [<Fact>]
    let ``divideScalar_Q: divides each element by scalar`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ Vector.divideScalar 10.0 v @>
        floatArrayClose [| 1.0; 2.0; 3.0 |] result 1e-10

    [<Fact>]
    let ``divideScalar_Q: works with fractional divisor`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.divideScalar 2.0 v @>
        floatArrayClose [| 0.5; 1.0; 1.5 |] result 1e-10

    // ========================================
    // Quotation tests for sum
    // ========================================

    [<Fact>]
    let ``sum_Q: computes sum of all elements`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Vector.sum v @>
        floatEqual 10.0 result 1e-10

    [<Fact>]
    let ``sum_Q: works with empty vector`` () =
        let v = Array.empty<float>
        let result = evalQ <@ Vector.sum v @>
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``sum_Q: works with negative values`` () =
        let v = [| -1.0; 2.0; -3.0; 4.0 |]
        let result = evalQ <@ Vector.sum v @>
        floatEqual 2.0 result 1e-10

    // ========================================
    // Quotation tests for product
    // ========================================

    [<Fact>]
    let ``product_Q: multiplies all elements`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.product v @>
        floatEqual 6.0 result 1e-10

    [<Fact>]
    let ``product_Q: works with zeros`` () =
        let v = [| 1.0; 0.0; 3.0 |]
        let result = evalQ <@ Vector.product v @>
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``product_Q: works with empty vector`` () =
        let v = Array.empty<float>
        let result = evalQ <@ Vector.product v @>
        floatEqual 1.0 result 1e-10

    // ========================================
    // Quotation tests for mean
    // ========================================

    [<Fact>]
    let ``mean_Q: computes mean of elements`` () =
        let v = [| 2.0; 4.0; 6.0; 8.0 |]
        let result = evalQ <@ Vector.mean v @>
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``mean_Q: works with single element`` () =
        let v = [| 42.0 |]
        let result = evalQ <@ Vector.mean v @>
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``mean_Q: throws on empty vector`` () =
        let v = Array.empty<float>
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.mean v @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for dot
    // ========================================

    [<Fact>]
    let ``dot_Q: computes dot product of two vectors`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ Vector.dot v1 v2 @>
        floatEqual 32.0 result 1e-10

    [<Fact>]
    let ``dot_Q: works with empty vectors`` () =
        let v1 = Array.empty<float>
        let v2 = Array.empty<float>
        let result = evalQ <@ Vector.dot v1 v2 @>
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``dot_Q: works with orthogonal vectors`` () =
        let v1 = [| 1.0; 0.0 |]
        let v2 = [| 0.0; 1.0 |]
        let result = evalQ <@ Vector.dot v1 v2 @>
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``dot_Q: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.dot v1 v2 @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for norm
    // ========================================

    [<Fact>]
    let ``norm_Q: computes Euclidean norm`` () =
        let v = [| 3.0; 4.0 |]
        let result = evalQ <@ Vector.norm v @>
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``norm_Q: works with single element`` () =
        let v = [| 7.0 |]
        let result = evalQ <@ Vector.norm v @>
        floatEqual 7.0 result 1e-10

    [<Fact>]
    let ``norm_Q: works with zero vector`` () =
        let v = [| 0.0; 0.0; 0.0 |]
        let result = evalQ <@ Vector.norm v @>
        floatEqual 0.0 result 1e-10

    // ========================================
    // Quotation tests for min
    // ========================================

    [<Fact>]
    let ``min_Q: finds minimum value`` () =
        let v = [| 5.0; 1.0; 3.0; 0.5; 2.0 |]
        let result = evalQ <@ Vector.min v @>
        floatEqual 0.5 result 1e-10

    [<Fact>]
    let ``min_Q: works with single element`` () =
        let v = [| 42.0 |]
        let result = evalQ <@ Vector.min v @>
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``min_Q: handles negative values`` () =
        let v = [| -5.0; -1.0; -10.0; -3.0 |]
        let result = evalQ <@ Vector.min v @>
        floatEqual -10.0 result 1e-10

    // ========================================
    // Quotation tests for max
    // ========================================

    [<Fact>]
    let ``max_Q: finds maximum value`` () =
        let v = [| 1.0; 5.0; 3.0; 7.0; 2.0 |]
        let result = evalQ <@ Vector.max v @>
        floatEqual 7.0 result 1e-10

    [<Fact>]
    let ``max_Q: works with single element`` () =
        let v = [| 42.0 |]
        let result = evalQ <@ Vector.max v @>
        floatEqual 42.0 result 1e-10

    [<Fact>]
    let ``max_Q: handles negative values`` () =
        let v = [| -5.0; -1.0; -10.0; -3.0 |]
        let result = evalQ <@ Vector.max v @>
        floatEqual -1.0 result 1e-10
