namespace FsMath.Tests.LinearAlgebraTests

open System
open Xunit
open FsMath
open FsMath.Algebra
open Microsoft.FSharp.Linq.RuntimeHelpers

open FsMath.Tests
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle


/// <summary>
/// Tests for Householder functions using F# quotation evaluation.
/// This technique allows coverage tools to track execution of inline functions
/// by evaluating them through quotations rather than direct calls.
///
/// Based on feedback: "Since F# 8.0 it should be possible to cover inline functions
/// by using quotation evaluation to dynamically invoke them."
/// </summary>
module HouseholderQuotationTests =

    /// <summary>
    /// Helper to evaluate a quotation and return the result.
    /// This forces the inline function to be evaluated in a way that coverage tools can track.
    /// </summary>
    let inline eval (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        LeafExpressionConverter.EvaluateQuotation expr :?> 'T

    [<Fact>]
    let ``create via quotation: simple vector [3, 0, 0]`` () =
        let x = [| 3.0; 0.0; 0.0 |]
        // Direct call (not tracked by coverage)
        let hDirect = Householder.create x
        // Quotation call (tracked by coverage)
        let hQuotation = eval <@ Householder.create x @>

        // Both should produce the same results
        floatEqual hDirect.Beta hQuotation.Beta 1e-10
        floatEqual hDirect.Tau hQuotation.Tau 1e-10
        floatEqual hDirect.V.[0] hQuotation.V.[0] 1e-10

        // Verify expected values
        floatEqual hQuotation.Beta 3.0 1e-10
        floatEqual hQuotation.Tau 0.0 1e-10
        floatEqual hQuotation.V.[0] 1.0 1e-10

    [<Fact>]
    let ``create via quotation: vector [1, 1, 1]`` () =
        let x = [| 1.0; 1.0; 1.0 |]
        let h = eval <@ Householder.create x @>

        floatClose Accuracy.medium h.Beta (sqrt 3.0) "Beta should be sqrt(3)"
        floatEqual h.V.[0] 1.0 1e-10
        Assert.True(h.Tau > 0.0 && h.Tau <= 2.0, $"Tau should be in (0, 2], got {h.Tau}")

    [<Fact>]
    let ``create via quotation: vector [4, 3]`` () =
        let x = [| 4.0; 3.0 |]
        let h = eval <@ Householder.create x @>

        floatEqual h.Beta 5.0 1e-10
        Assert.True(h.Tau > 0.0, $"Tau should be positive, got {h.Tau}")
        floatEqual h.V.[0] 1.0 1e-10

    [<Fact>]
    let ``create via quotation: negative first element`` () =
        let x = [| -2.0; 1.0; 1.0 |]
        let h = eval <@ Householder.create x @>

        floatClose Accuracy.medium h.Beta (sqrt 6.0) "Beta should be sqrt(6)"
        floatEqual h.V.[0] 1.0 1e-10

    [<Fact>]
    let ``create via quotation: single element vector`` () =
        let x = [| 5.0 |]
        let h = eval <@ Householder.create x @>

        floatEqual h.Beta 5.0 1e-10
        floatEqual h.Tau 0.0 1e-10
        floatEqual h.V.[0] 1.0 1e-10

    [<Fact>]
    let ``create via quotation: large vector`` () =
        let x = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let h = eval <@ Householder.create x @>

        floatClose Accuracy.medium h.Beta (sqrt 55.0) "Beta should be sqrt(55)"
        floatEqual h.V.[0] 1.0 1e-10

    [<Fact>]
    let ``create via quotation: zero tail`` () =
        let x = [| 7.0; 0.0; 0.0; 0.0 |]
        let h = eval <@ Householder.create x @>

        floatEqual h.Tau 0.0 1e-10
        floatEqual h.Beta 7.0 1e-10

    [<Fact>]
    let ``create via quotation: mixed signs`` () =
        let x = [| 3.0; -4.0; 0.0 |]
        let h = eval <@ Householder.create x @>

        floatEqual h.Beta 5.0 1e-10
        floatEqual h.V.[0] 1.0 1e-10
        Assert.True(h.Tau > 0.0, $"Tau should be positive, got {h.Tau}")

    [<Fact>]
    let ``create via quotation: small values`` () =
        let x = [| 1e-10; 2e-10; 3e-10 |]
        let h = eval <@ Householder.create x @>

        let expectedBeta = sqrt(14e-20)
        floatClose Accuracy.low h.Beta expectedBeta "Beta should handle small values"

    [<Fact>]
    let ``create via quotation: larger vector for stress test`` () =
        let x = [| for i in 1 .. 10 -> float i |]
        let h = eval <@ Householder.create x @>

        // Sum of squares: 1 + 4 + 9 + ... + 100 = 385
        let expectedBeta = sqrt 385.0
        floatClose Accuracy.medium h.Beta expectedBeta "Beta should be sqrt(385)"
        floatEqual h.V.[0] 1.0 1e-10
        Assert.True(h.Tau > 0.0, "Tau should be positive")

    [<Fact>]
    let ``applyLeft with quotation-created Householder: identity transformation`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0 |])
        let h = { V = [| 1.0; 0.0; 0.0 |]; Tau = 0.0; Beta = 1.0 }
        let A' = A |> Matrix.copy

        // applyLeft returns unit, so we can't use quotation eval on it directly
        // But we can still test it works with a quotation-created Householder
        Householder.applyLeft(h, A', 0)

        // With tau=0, matrix should be unchanged
        floatMatrixClose Accuracy.high A' A "Matrix should be unchanged with tau=0"

    [<Fact>]
    let ``applyLeft with quotation-created Householder: transforms first column`` () =
        let A = Matrix(3, 2, [| 1.0; 1.0;
                               1.0; 2.0;
                               1.0; 3.0 |])
        let x = Matrix.getCol 0 A
        // Use quotation for create
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyLeft(h, A', 0)

        // First column should become [beta, 0, 0]
        floatClose Accuracy.medium A'.[0, 0] h.Beta "A'[0,0] should be beta"
        floatClose Accuracy.medium A'.[1, 0] 0.0 "A'[1,0] should be zero"
        floatClose Accuracy.medium A'.[2, 0] 0.0 "A'[2,0] should be zero"

    [<Fact>]
    let ``applyLeft with quotation-created Householder: preserves norm`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0 |])
        let x = Matrix.getCol 0 A
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyLeft(h, A', 0)

        // Column norms should be preserved
        let col0Norm = Vector.norm (Matrix.getCol 0 A)
        let col0NormAfter = Vector.norm (Matrix.getCol 0 A')
        floatClose Accuracy.medium col0NormAfter col0Norm "Column norm should be preserved"

    [<Fact>]
    let ``applyLeft with quotation-created Householder: with row offset`` () =
        let A = Matrix(4, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0;
                               10.0; 11.0; 12.0 |])
        let x = [| 5.0; 8.0; 11.0 |]
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyLeft(h, A', 1)

        // First row should be unchanged
        floatEqual A'.[0, 0] 1.0 1e-10
        floatEqual A'.[0, 1] 2.0 1e-10
        floatEqual A'.[0, 2] 3.0 1e-10

    [<Fact>]
    let ``applyRight with quotation-created Householder: identity transformation`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0 |])
        let h = { V = [| 1.0; 0.0; 0.0 |]; Tau = 0.0; Beta = 1.0 }
        let A' = A |> Matrix.copy

        Householder.applyRight(h, A', 0)

        // With tau=0, matrix should be unchanged
        floatMatrixClose Accuracy.high A' A "Matrix should be unchanged with tau=0"

    [<Fact>]
    let ``applyRight with quotation-created Householder: transforms first row`` () =
        let A = Matrix(2, 3, [| 1.0; 1.0; 1.0;
                               2.0; 2.0; 2.0 |])
        let x = Matrix.getRow 0 A
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyRight(h, A', 0)

        // First row should become [beta, 0, 0]
        floatClose Accuracy.medium A'.[0, 0] h.Beta "A'[0,0] should be beta"
        floatClose Accuracy.medium A'.[0, 1] 0.0 "A'[0,1] should be zero"
        floatClose Accuracy.medium A'.[0, 2] 0.0 "A'[0,2] should be zero"

    [<Fact>]
    let ``applyRight with quotation-created Householder: preserves norm`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0 |])
        let x = Matrix.getRow 0 A
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyRight(h, A', 0)

        // Row norms should be preserved
        let row0Norm = Vector.norm (Matrix.getRow 0 A)
        let row0NormAfter = Vector.norm (Matrix.getRow 0 A')
        floatClose Accuracy.medium row0NormAfter row0Norm "Row norm should be preserved"

    [<Fact>]
    let ``applyRight with quotation-created Householder: with column offset`` () =
        let A = Matrix(3, 4, [| 1.0; 2.0; 3.0; 4.0;
                               5.0; 6.0; 7.0; 8.0;
                               9.0; 10.0; 11.0; 12.0 |])
        let x = [| 2.0; 3.0; 4.0 |]
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyRight(h, A', 1)

        // First column should be unchanged
        floatEqual A'.[0, 0] 1.0 1e-10
        floatEqual A'.[1, 0] 5.0 1e-10
        floatEqual A'.[2, 0] 9.0 1e-10

    [<Fact>]
    let ``Householder involution with quotation-created Householder`` () =
        let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               4.0; 5.0; 6.0;
                               7.0; 8.0; 9.0 |])
        let x = Matrix.getCol 0 A
        let h = eval <@ Householder.create x @>
        let A' = A |> Matrix.copy

        Householder.applyLeft(h, A', 0)
        Householder.applyLeft(h, A', 0)

        // Applying twice should return to original
        floatMatrixClose Accuracy.low A' A "Applying Householder twice should give original"

    [<Fact>]
    let ``Multiple quotation evaluations produce consistent results`` () =
        let x = [| 2.0; 3.0; 4.0 |]

        // Evaluate multiple times
        let h1 = eval <@ Householder.create x @>
        let h2 = eval <@ Householder.create x @>
        let h3 = eval <@ Householder.create x @>

        // All should be identical
        floatEqual h1.Beta h2.Beta 1e-15
        floatEqual h2.Beta h3.Beta 1e-15
        floatEqual h1.Tau h2.Tau 1e-15
        floatEqual h2.Tau h3.Tau 1e-15

        for i in 0 .. h1.V.Length - 1 do
            floatEqual h1.V.[i] h2.V.[i] 1e-15
            floatEqual h2.V.[i] h3.V.[i] 1e-15
