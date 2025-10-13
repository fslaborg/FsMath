namespace FsMath.Tests.VectorOps

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FSharp.Linq.RuntimeHelpers

/// <summary>
/// Quotation-based tests for VectorOps inline functions and operators.
/// These tests use F# quotation evaluation to ensure that inline functions
/// are actually executed during test coverage measurement.
///
/// Background: F# inline functions are not tracked by coverage tools because
/// they are inlined at compile time. Using quotations forces the code to be
/// evaluated dynamically, which allows coverage tools to track execution.
///
/// See: https://github.com/fslaborg/FsMath/discussions/5
/// Maintainer recommendation: Use quotation evaluation technique for inline functions
/// </summary>
module VectorOpsQuotationTests =

    /// Helper to evaluate quotations that return a value
    let inline evalQ<'T> (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        LeafExpressionConverter.EvaluateQuotation expr :?> 'T

    // ========================================
    // Quotation tests for VectorOps.Plus (static member)
    // ========================================

    [<Fact>]
    let ``VectorOps.Plus_Q: vector + vector (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ VectorOps.Plus(v1, v2) @>
        floatArrayClose [| 5.0; 7.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Plus_Q: vector + scalar (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ VectorOps.Plus(v, 10.0) @>
        floatArrayClose [| 11.0; 12.0; 13.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Plus_Q: scalar + vector (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ VectorOps.Plus(10.0, v) @>
        floatArrayClose [| 11.0; 12.0; 13.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Plus_Q: vector + vector (int)`` () =
        let v1 = [| 1; 2; 3 |]
        let v2 = [| 4; 5; 6 |]
        let result = evalQ <@ VectorOps.Plus(v1, v2) @>
        Assert.Equal<Vector<int>>([| 5; 7; 9 |], result)

    [<Fact>]
    let ``VectorOps.Plus_Q: empty vectors`` () =
        let v1 = Array.empty<float>
        let v2 = Array.empty<float>
        let result = evalQ <@ VectorOps.Plus(v1, v2) @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for VectorOps.Minus (static member)
    // ========================================

    [<Fact>]
    let ``VectorOps.Minus_Q: vector - vector (float)`` () =
        let v1 = [| 10.0; 20.0; 30.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ VectorOps.Minus(v1, v2) @>
        floatArrayClose [| 9.0; 18.0; 27.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Minus_Q: vector - scalar (float)`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ VectorOps.Minus(v, 5.0) @>
        floatArrayClose [| 5.0; 15.0; 25.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Minus_Q: vector - vector (int)`` () =
        let v1 = [| 10; 20; 30 |]
        let v2 = [| 1; 2; 3 |]
        let result = evalQ <@ VectorOps.Minus(v1, v2) @>
        Assert.Equal<Vector<int>>([| 9; 18; 27 |], result)

    [<Fact>]
    let ``VectorOps.Minus_Q: results in zero vector`` () =
        let v = [| 5.0; 10.0; 15.0 |]
        let result = evalQ <@ VectorOps.Minus(v, v) @>
        floatArrayClose [| 0.0; 0.0; 0.0 |] result 1e-10

    // ========================================
    // Quotation tests for VectorOps.Multiply (static member)
    // ========================================

    [<Fact>]
    let ``VectorOps.Multiply_Q: vector * vector (float)`` () =
        let v1 = [| 2.0; 3.0; 4.0 |]
        let v2 = [| 5.0; 6.0; 7.0 |]
        let result = evalQ <@ VectorOps.Multiply(v1, v2) @>
        floatArrayClose [| 10.0; 18.0; 28.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Multiply_Q: vector * scalar (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ VectorOps.Multiply(v, 2.5) @>
        floatArrayClose [| 2.5; 5.0; 7.5 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Multiply_Q: scalar * vector (float)`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ VectorOps.Multiply(2.5, v) @>
        floatArrayClose [| 2.5; 5.0; 7.5 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Multiply_Q: vector * vector (int)`` () =
        let v1 = [| 2; 3; 4 |]
        let v2 = [| 5; 6; 7 |]
        let result = evalQ <@ VectorOps.Multiply(v1, v2) @>
        Assert.Equal<Vector<int>>([| 10; 18; 28 |], result)

    [<Fact>]
    let ``VectorOps.Multiply_Q: by zero scalar`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ VectorOps.Multiply(v, 0.0) @>
        floatArrayClose [| 0.0; 0.0; 0.0 |] result 1e-10

    // ========================================
    // Quotation tests for VectorOps.Divide (static member)
    // ========================================

    [<Fact>]
    let ``VectorOps.Divide_Q: vector / vector (float)`` () =
        let v1 = [| 20.0; 30.0; 40.0 |]
        let v2 = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ VectorOps.Divide(v1, v2) @>
        floatArrayClose [| 10.0; 10.0; 10.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Divide_Q: vector / scalar (float)`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ VectorOps.Divide(v, 5.0) @>
        floatArrayClose [| 2.0; 4.0; 6.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Divide_Q: vector / vector (int)`` () =
        let v1 = [| 20; 30; 40 |]
        let v2 = [| 2; 3; 4 |]
        let result = evalQ <@ VectorOps.Divide(v1, v2) @>
        Assert.Equal<Vector<int>>([| 10; 10; 10 |], result)

    [<Fact>]
    let ``VectorOps.Divide_Q: by one gives identity`` () =
        let v = [| 5.0; 10.0; 15.0 |]
        let result = evalQ <@ VectorOps.Divide(v, 1.0) @>
        floatArrayClose v result 1e-10

    // ========================================
    // Quotation tests for VectorOps.Power (static member)
    // ========================================

    [<Fact>]
    let ``VectorOps.Power_Q: vector ^ scalar (float)`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ VectorOps.Power(v, 2.0) @>
        floatArrayClose [| 4.0; 9.0; 16.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Power_Q: power of 0`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ VectorOps.Power(v, 0.0) @>
        floatArrayClose [| 1.0; 1.0; 1.0 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Power_Q: negative power`` () =
        let v = [| 2.0; 4.0; 8.0 |]
        let result = evalQ <@ VectorOps.Power(v, -1.0) @>
        floatArrayClose [| 0.5; 0.25; 0.125 |] result 1e-10

    [<Fact>]
    let ``VectorOps.Power_Q: fractional power`` () =
        let v = [| 4.0; 9.0; 16.0 |]
        let result = evalQ <@ VectorOps.Power(v, 0.5) @>
        floatArrayClose [| 2.0; 3.0; 4.0 |] result 1e-10

    // ========================================
    // Quotation tests for VectorOps.Dot (static member)
    // ========================================

    [<Fact>]
    let ``VectorOps.Dot_Q: standard dot product (float)`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ VectorOps.Dot(v1, v2) @>
        floatEqual 32.0 result 1e-10

    [<Fact>]
    let ``VectorOps.Dot_Q: orthogonal vectors`` () =
        let v1 = [| 1.0; 0.0; 0.0 |]
        let v2 = [| 0.0; 1.0; 0.0 |]
        let result = evalQ <@ VectorOps.Dot(v1, v2) @>
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``VectorOps.Dot_Q: dot with itself (int)`` () =
        let v = [| 3; 4 |]
        let result = evalQ <@ VectorOps.Dot(v, v) @>
        Assert.Equal(25, result)

    [<Fact>]
    let ``VectorOps.Dot_Q: single element vectors`` () =
        let v1 = [| 5.0 |]
        let v2 = [| 3.0 |]
        let result = evalQ <@ VectorOps.Dot(v1, v2) @>
        floatEqual 15.0 result 1e-10

    // ========================================
    // Quotation tests for Plus dispatcher type
    // ========================================

    [<Fact>]
    let ``Plus.Invoke_Q: dispatches vector + vector`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ Plus.Invoke(v1, v2) @>
        floatArrayClose [| 5.0; 7.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``Plus.Invoke_Q: dispatches scalar + vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Plus.Invoke(5.0, v) @>
        floatArrayClose [| 6.0; 7.0; 8.0 |] result 1e-10

    // ========================================
    // Quotation tests for Minus dispatcher type
    // ========================================

    [<Fact>]
    let ``Minus.Invoke_Q: dispatches vector - vector`` () =
        let v1 = [| 10.0; 20.0; 30.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Minus.Invoke(v1, v2) @>
        floatArrayClose [| 9.0; 18.0; 27.0 |] result 1e-10

    [<Fact>]
    let ``Minus.Invoke_Q: dispatches vector - scalar`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ Minus.Invoke(v, 5.0) @>
        floatArrayClose [| 5.0; 15.0; 25.0 |] result 1e-10

    // ========================================
    // Quotation tests for Multiply dispatcher type
    // ========================================

    [<Fact>]
    let ``Multiply.Invoke_Q: dispatches vector * vector`` () =
        let v1 = [| 2.0; 3.0; 4.0 |]
        let v2 = [| 5.0; 6.0; 7.0 |]
        let result = evalQ <@ Multiply.Invoke(v1, v2) @>
        floatArrayClose [| 10.0; 18.0; 28.0 |] result 1e-10

    [<Fact>]
    let ``Multiply.Invoke_Q: dispatches scalar * vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Multiply.Invoke(3.0, v) @>
        floatArrayClose [| 3.0; 6.0; 9.0 |] result 1e-10

    // ========================================
    // Quotation tests for Divide dispatcher type
    // ========================================

    [<Fact>]
    let ``Divide.Invoke_Q: dispatches vector / vector`` () =
        let v1 = [| 20.0; 30.0; 40.0 |]
        let v2 = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Divide.Invoke(v1, v2) @>
        floatArrayClose [| 10.0; 10.0; 10.0 |] result 1e-10

    [<Fact>]
    let ``Divide.Invoke_Q: dispatches vector / scalar`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ Divide.Invoke(v, 2.0) @>
        floatArrayClose [| 5.0; 10.0; 15.0 |] result 1e-10

    // ========================================
    // Quotation tests for Power dispatcher type
    // ========================================

    [<Fact>]
    let ``Power.Invoke_Q: dispatches vector ^ power`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Power.Invoke(v, 3.0) @>
        floatArrayClose [| 8.0; 27.0; 64.0 |] result 1e-10

    [<Fact>]
    let ``Power.Invoke_Q: dispatches with float type`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Power.Invoke(v, 2.0) @>
        floatArrayClose [| 4.0; 9.0; 16.0 |] result 1e-10

    // ========================================
    // Quotation tests for Dot dispatcher type
    // ========================================

    [<Fact>]
    let ``Dot.Invoke_Q: dispatches dot product`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ Dot.Invoke(v1, v2) @>
        floatEqual 32.0 result 1e-10

    [<Fact>]
    let ``Dot.Invoke_Q: dispatches for integer vectors`` () =
        let v1 = [| 1; 2; 3 |]
        let v2 = [| 4; 5; 6 |]
        let result = evalQ <@ Dot.Invoke(v1, v2) @>
        Assert.Equal(32, result)

    // ========================================
    // Quotation tests for operator symbols
    // ========================================

    [<Fact>]
    let ``operator .+_Q: vector + vector`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ v1 .+ v2 @>
        floatArrayClose [| 5.0; 7.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``operator .-_Q: vector - vector`` () =
        let v1 = [| 10.0; 20.0; 30.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ v1 .- v2 @>
        floatArrayClose [| 9.0; 18.0; 27.0 |] result 1e-10

    [<Fact>]
    let ``operator .*_Q: vector * vector`` () =
        let v1 = [| 2.0; 3.0; 4.0 |]
        let v2 = [| 5.0; 6.0; 7.0 |]
        let result = evalQ <@ v1 .* v2 @>
        floatArrayClose [| 10.0; 18.0; 28.0 |] result 1e-10

    [<Fact>]
    let ``operator ./_Q: vector / vector`` () =
        let v1 = [| 20.0; 30.0; 40.0 |]
        let v2 = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ v1 ./ v2 @>
        floatArrayClose [| 10.0; 10.0; 10.0 |] result 1e-10

    [<Fact>]
    let ``operator .^_Q: vector ^ power`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ v .^ 2.0 @>
        floatArrayClose [| 4.0; 9.0; 16.0 |] result 1e-10

    [<Fact>]
    let ``operator .+_Q: scalar + vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ 10.0 .+ v @>
        floatArrayClose [| 11.0; 12.0; 13.0 |] result 1e-10

    [<Fact>]
    let ``operator .*_Q: scalar * vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ 2.5 .* v @>
        floatArrayClose [| 2.5; 5.0; 7.5 |] result 1e-10

    [<Fact>]
    let ``operator .+_Q: vector + scalar`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ v .+ 10.0 @>
        floatArrayClose [| 11.0; 12.0; 13.0 |] result 1e-10

    [<Fact>]
    let ``operator .-_Q: vector - scalar`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ v .- 5.0 @>
        floatArrayClose [| 5.0; 15.0; 25.0 |] result 1e-10

    [<Fact>]
    let ``operator .*_Q: vector * scalar`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ v .* 3.0 @>
        floatArrayClose [| 3.0; 6.0; 9.0 |] result 1e-10

    [<Fact>]
    let ``operator ./_Q: vector / scalar`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ v ./ 5.0 @>
        floatArrayClose [| 2.0; 4.0; 6.0 |] result 1e-10

    // ========================================
    // Edge cases
    // ========================================

    [<Fact>]
    let ``operators_Q: work with negative values`` () =
        let v1 = [| -1.0; -2.0; -3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ v1 .+ v2 @>
        floatArrayClose [| 3.0; 3.0; 3.0 |] result 1e-10

    [<Fact>]
    let ``operators_Q: combined operations`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ (v1 .+ v2) .* 2.0 @>
        floatArrayClose [| 10.0; 14.0; 18.0 |] result 1e-10

    [<Fact>]
    let ``operators_Q: commutative property of addition`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result1 = evalQ <@ v1 .+ v2 @>
        let result2 = evalQ <@ v2 .+ v1 @>
        floatArrayClose result1 result2 1e-10

    [<Fact>]
    let ``operators_Q: distributive property`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let scalar = 2.5
        let result1 = evalQ <@ scalar .* (v1 .+ v2) @>
        let result2 = evalQ <@ (scalar .* v1) .+ (scalar .* v2) @>
        floatArrayClose result1 result2 1e-10
