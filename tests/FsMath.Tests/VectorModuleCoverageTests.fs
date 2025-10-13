namespace FsMath.Tests.VectorModule

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FSharp.Linq.RuntimeHelpers

/// <summary>
/// Quotation-based tests for VectorModule inline functions.
/// These tests use F# quotation evaluation to ensure that inline functions
/// are actually executed during test coverage measurement.
///
/// Background: F# inline functions are not tracked by coverage tools because
/// they are inlined at compile time. Using quotations forces the code to be
/// evaluated dynamically, which allows coverage tools to track execution.
///
/// See: https://github.com/fslaborg/FsMath/discussions/5
/// </summary>
module VectorModuleCoverageTests =

    /// Helper to evaluate quotations that return a value
    let inline evalQ<'T> (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        LeafExpressionConverter.EvaluateQuotation expr :?> 'T

    // ========================================
    // Quotation tests for foldi
    // ========================================

    [<Fact>]
    let ``foldi_Q: performs indexed fold`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Vector.foldi (fun i acc x -> acc + float i * x) 0.0 v @>
        floatEqual 20.0 result 1e-10

    [<Fact>]
    let ``foldi_Q: works with single element`` () =
        let v = [| 5.0 |]
        let result = evalQ <@ Vector.foldi (fun i acc x -> acc + x) 0.0 v @>
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``foldi_Q: works with empty vector`` () =
        let v = Array.empty<float>
        let result = evalQ <@ Vector.foldi (fun i acc x -> acc + x) 0.0 v @>
        floatEqual 0.0 result 1e-10

    // ========================================
    // Quotation tests for mapi
    // ========================================

    [<Fact>]
    let ``mapi_Q: applies function with index`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = evalQ <@ Vector.mapi (fun i x -> x + float i) v @>
        floatArrayClose [| 10.0; 21.0; 32.0 |] result 1e-10

    [<Fact>]
    let ``mapi_Q: works with empty vector`` () =
        let v = Array.empty<float>
        let result = evalQ <@ Vector.mapi (fun i x -> x * 2.0) v @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for filter
    // ========================================

    [<Fact>]
    let ``filter_Q: filters elements by predicate`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = evalQ <@ Vector.filter (fun x -> x > 2.5) v @>
        floatArrayClose [| 3.0; 4.0; 5.0 |] result 1e-10

    [<Fact>]
    let ``filter_Q: returns empty when nothing matches`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.filter (fun x -> x > 10.0) v @>
        Assert.Empty(result)

    [<Fact>]
    let ``filter_Q: returns all when everything matches`` () =
        let v = [| 5.0; 6.0; 7.0 |]
        let result = evalQ <@ Vector.filter (fun x -> x > 4.0) v @>
        floatArrayClose v result 1e-10

    // ========================================
    // Quotation tests for init
    // ========================================

    [<Fact>]
    let ``init_Q: creates vector from generator`` () =
        let result = evalQ <@ Vector.init 5 (fun i -> float i * 2.0) @>
        floatArrayClose [| 0.0; 2.0; 4.0; 6.0; 8.0 |] result 1e-10

    [<Fact>]
    let ``init_Q: creates empty vector with size 0`` () =
        let result = evalQ <@ Vector.init 0 (fun i -> float i) @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for slice
    // ========================================

    [<Fact>]
    let ``slice_Q: extracts portion of vector`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = evalQ <@ Vector.slice 1 3 v @>
        floatArrayClose [| 2.0; 3.0; 4.0 |] result 1e-10

    [<Fact>]
    let ``slice_Q: extracts single element`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Vector.slice 2 1 v @>
        floatArrayClose [| 3.0 |] result 1e-10

    // ========================================
    // Quotation tests for argmax
    // ========================================

    [<Fact>]
    let ``argmax_Q: finds index of maximum value`` () =
        let v = [| 1.0; 5.0; 3.0; 7.0; 2.0 |]
        let result = evalQ <@ Vector.argmax v @>
        Assert.Equal(3, result)

    [<Fact>]
    let ``argmax_Q: works with single element`` () =
        let v = [| 42.0 |]
        let result = evalQ <@ Vector.argmax v @>
        Assert.Equal(0, result)

    [<Fact>]
    let ``argmax_Q: handles negative values`` () =
        let v = [| -5.0; -1.0; -10.0; -3.0 |]
        let result = evalQ <@ Vector.argmax v @>
        Assert.Equal(1, result)

    // ========================================
    // Quotation tests for argmin
    // ========================================

    [<Fact>]
    let ``argmin_Q: finds index of minimum value`` () =
        let v = [| 5.0; 1.0; 3.0; 0.5; 2.0 |]
        let result = evalQ <@ Vector.argmin v @>
        Assert.Equal(3, result)

    [<Fact>]
    let ``argmin_Q: works with single element`` () =
        let v = [| 42.0 |]
        let result = evalQ <@ Vector.argmin v @>
        Assert.Equal(0, result)

    [<Fact>]
    let ``argmin_Q: handles negative values`` () =
        let v = [| -5.0; -1.0; -10.0; -3.0 |]
        let result = evalQ <@ Vector.argmin v @>
        Assert.Equal(2, result)

    // ========================================
    // Quotation tests for padRight
    // ========================================

    [<Fact>]
    let ``padRight_Q: pads vector to longer length`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.padRight 6 0.0 v @>
        floatArrayClose [| 1.0; 2.0; 3.0; 0.0; 0.0; 0.0 |] result 1e-10

    [<Fact>]
    let ``padRight_Q: returns original when already long enough`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.padRight 2 0.0 v @>
        Assert.Same(v, result)

    [<Fact>]
    let ``padRight_Q: pads with non-zero value`` () =
        let v = [| 1.0; 2.0 |]
        let result = evalQ <@ Vector.padRight 5 9.0 v @>
        floatArrayClose [| 1.0; 2.0; 9.0; 9.0; 9.0 |] result 1e-10

    // ========================================
    // Quotation tests for zip
    // ========================================

    [<Fact>]
    let ``zip_Q: combines two vectors`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = evalQ <@ Vector.zip v1 v2 @>
        Assert.Equal((1.0, 4.0), result.[0])
        Assert.Equal((2.0, 5.0), result.[1])
        Assert.Equal((3.0, 6.0), result.[2])

    [<Fact>]
    let ``zip_Q: handles empty vectors`` () =
        let v1 = Array.empty<float>
        let v2 = Array.empty<float>
        let result = evalQ <@ Vector.zip v1 v2 @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for argminBy
    // ========================================

    [<Fact>]
    let ``argminBy_Q: finds index of minimum projected value`` () =
        let v = [| 3.0; -5.0; 2.0; -8.0 |]
        let result = evalQ <@ Vector.argminBy abs v @>
        Assert.Equal(2, result)

    [<Fact>]
    let ``argminBy_Q: returns first when multiple equal minima`` () =
        let v = [| 3.0; -2.0; 2.0; -2.0 |]
        let result = evalQ <@ Vector.argminBy abs v @>
        Assert.Equal(1, result)

    // ========================================
    // Quotation tests for argmaxBy
    // ========================================

    [<Fact>]
    let ``argmaxBy_Q: finds index of maximum projected value`` () =
        let v = [| 3.0; -5.0; 2.0; -8.0 |]
        let result = evalQ <@ Vector.argmaxBy abs v @>
        Assert.Equal(3, result)

    [<Fact>]
    let ``argmaxBy_Q: returns first when multiple equal maxima`` () =
        let v = [| 3.0; -5.0; 5.0; -5.0 |]
        let result = evalQ <@ Vector.argmaxBy abs v @>
        Assert.Equal(1, result)

    // ========================================
    // Quotation tests for tryFindIndex
    // ========================================

    [<Fact>]
    let ``tryFindIndex_Q: finds first matching element`` () =
        let v = [| 1.0; 3.0; 5.0; 7.0 |]
        let result = evalQ <@ Vector.tryFindIndex (fun x -> x > 4.0) v @>
        Assert.Equal(Some 2, result)

    [<Fact>]
    let ``tryFindIndex_Q: returns None when no match`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.tryFindIndex (fun x -> x > 10.0) v @>
        Assert.Equal(None, result)

    [<Fact>]
    let ``tryFindIndex_Q: finds at index 0`` () =
        let v = [| 5.0; 3.0; 1.0 |]
        let result = evalQ <@ Vector.tryFindIndex (fun x -> x > 4.0) v @>
        Assert.Equal(Some 0, result)

    // ========================================
    // Quotation tests for findIndex
    // ========================================

    [<Fact>]
    let ``findIndex_Q: finds first matching element`` () =
        let v = [| 1.0; 3.0; 5.0; 7.0 |]
        let result = evalQ <@ Vector.findIndex (fun x -> x > 4.0) v @>
        Assert.Equal(2, result)

    [<Fact>]
    let ``findIndex_Q: throws when no match`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<Exception>(fun () -> evalQ <@ Vector.findIndex (fun x -> x > 10.0) v @> |> ignore) |> ignore

    // ========================================
    // Quotation tests for enumerateNonZero
    // ========================================

    [<Fact>]
    let ``enumerateNonZero_Q: extracts non-zero elements`` () =
        let v = [| 0.0; 2.0; 0.0; 4.0; 5.0 |]
        let result = evalQ <@ Vector.enumerateNonZero v @>
        Assert.Equal(3, result.Length)
        Assert.Equal((1, 2.0), result.[0])
        Assert.Equal((3, 4.0), result.[1])
        Assert.Equal((4, 5.0), result.[2])

    [<Fact>]
    let ``enumerateNonZero_Q: returns empty for all zeros`` () =
        let v = [| 0.0; 0.0; 0.0 |]
        let result = evalQ <@ Vector.enumerateNonZero v @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for split
    // ========================================

    [<Fact>]
    let ``split_Q: splits vector at index`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let left, right = evalQ <@ Vector.split 2 v @>
        floatArrayClose [| 1.0; 2.0 |] left 1e-10
        floatArrayClose [| 3.0; 4.0; 5.0 |] right 1e-10

    [<Fact>]
    let ``split_Q: handles split at beginning`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let left, right = evalQ <@ Vector.split 0 v @>
        Assert.Empty(left)
        floatArrayClose [| 1.0; 2.0; 3.0 |] right 1e-10

    // ========================================
    // Quotation tests for chunk
    // ========================================

    [<Fact>]
    let ``chunk_Q: splits vector into chunks`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0 |]
        let result = evalQ <@ Vector.chunk 3 v @>
        Assert.Equal(3, result.Length)
        floatArrayClose [| 1.0; 2.0; 3.0 |] result.[0] 1e-10
        floatArrayClose [| 4.0; 5.0; 6.0 |] result.[1] 1e-10
        floatArrayClose [| 7.0 |] result.[2] 1e-10

    [<Fact>]
    let ``chunk_Q: handles chunk size of 1`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.chunk 1 v @>
        Assert.Equal(3, result.Length)

    // ========================================
    // Quotation tests for windowed
    // ========================================

    [<Fact>]
    let ``windowed_Q: creates sliding windows`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = evalQ <@ Vector.windowed 3 v @>
        Assert.Equal(3, result.Length)
        floatArrayClose [| 1.0; 2.0; 3.0 |] result.[0] 1e-10
        floatArrayClose [| 2.0; 3.0; 4.0 |] result.[1] 1e-10
        floatArrayClose [| 3.0; 4.0; 5.0 |] result.[2] 1e-10

    [<Fact>]
    let ``windowed_Q: returns empty when window too large`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.windowed 5 v @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for ofSeq
    // ========================================

    [<Fact>]
    let ``ofSeq_Q: converts sequence to vector`` () =
        let seq = seq { 1.0; 2.0; 3.0 }
        let result = evalQ <@ Vector.ofSeq seq @>
        floatArrayClose [| 1.0; 2.0; 3.0 |] result 1e-10

    [<Fact>]
    let ``ofSeq_Q: handles empty sequence`` () =
        let seq = Seq.empty<float>
        let result = evalQ <@ Vector.ofSeq seq @>
        Assert.Empty(result)

    // ========================================
    // Quotation tests for pow
    // ========================================

    [<Fact>]
    let ``pow_Q: raises each element to power`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Vector.pow 2.0 v @>
        floatArrayClose [| 4.0; 9.0; 16.0 |] result 1e-10

    [<Fact>]
    let ``pow_Q: handles power of 0`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = evalQ <@ Vector.pow 0.0 v @>
        floatArrayClose [| 1.0; 1.0; 1.0 |] result 1e-10

    [<Fact>]
    let ``pow_Q: handles negative power`` () =
        let v = [| 2.0; 4.0; 8.0 |]
        let result = evalQ <@ Vector.pow -1.0 v @>
        floatArrayClose [| 0.5; 0.25; 0.125 |] result 1e-10

    // ========================================
    // Quotation tests for sub
    // ========================================

    [<Fact>]
    let ``sub_Q: extracts subvector from index to end`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = evalQ <@ Vector.sub 2 v @>
        floatArrayClose [| 3.0; 4.0; 5.0 |] result 1e-10

    [<Fact>]
    let ``sub_Q: throws on negative index`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> evalQ <@ Vector.sub -1 v @> |> ignore) |> ignore

    [<Fact>]
    let ``sub_Q: returns empty when starting at end`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = evalQ <@ Vector.sub 3 v @>
        Assert.Empty(result)
