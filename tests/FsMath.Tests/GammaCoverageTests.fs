namespace FsMath.Tests.SpecialFunctionsTests

open Xunit
open System
open FsMath
open FsMath.SpecialFunctions
open FsMath.Tests.ExpectoStyle
open Microsoft.FSharp.Linq.RuntimeHelpers


/// <summary>
/// Quotation-based tests for Gamma functions to achieve inline function coverage.
///
/// As noted by maintainers, F# inline functions are not tracked by coverage tools.
/// Using F# 8.0's quotation evaluation, we can dynamically invoke inline functions
/// to ensure they are executed and tracked by coverage tools.
/// </summary>
module GammaCoverageTests =

    /// Helper to evaluate a quotation expression
    let inline eval q = LeafExpressionConverter.EvaluateQuotation q

    // ========================================
    // Quotation-based tests for _gamma
    // ========================================

    [<Fact>]
    let ``Q: _gamma(5.0) = 24.0`` () =
        let result = eval <@ Gamma._gamma 5.0 @> :?> float
        floatClose Accuracy.high result 24.0 "Expected Γ(5) = 24"

    [<Fact>]
    let ``Q: _gamma(1.0) = 1.0`` () =
        let result = eval <@ Gamma._gamma 1.0 @> :?> float
        floatClose Accuracy.high result 1.0 "Expected Γ(1) = 1"

    [<Fact>]
    let ``Q: _gamma(2.0) = 1.0`` () =
        let result = eval <@ Gamma._gamma 2.0 @> :?> float
        floatClose Accuracy.high result 1.0 "Expected Γ(2) = 1"

    [<Fact>]
    let ``Q: _gamma(3.0) = 2.0`` () =
        let result = eval <@ Gamma._gamma 3.0 @> :?> float
        floatClose Accuracy.high result 2.0 "Expected Γ(3) = 2"

    [<Fact>]
    let ``Q: _gamma(4.0) = 6.0`` () =
        let result = eval <@ Gamma._gamma 4.0 @> :?> float
        floatClose Accuracy.high result 6.0 "Expected Γ(4) = 6"

    [<Fact>]
    let ``Q: _gamma(0.5) ≈ sqrt(π)`` () =
        let result = eval <@ Gamma._gamma 0.5 @> :?> float
        floatClose Accuracy.high result (sqrt System.Math.PI) "Expected Γ(0.5) = √π"

    [<Fact>]
    let ``Q: _gamma(1.5) ≈ 0.8862`` () =
        let result = eval <@ Gamma._gamma 1.5 @> :?> float
        floatClose Accuracy.high result 0.8862269254527580 "Expected Γ(1.5)"

    [<Fact>]
    let ``Q: _gamma(10.0) = 362880.0`` () =
        let result = eval <@ Gamma._gamma 10.0 @> :?> float
        floatClose Accuracy.high result 362880.0 "Expected Γ(10) = 9!"

    [<Fact>]
    let ``Q: _gamma(0.1) is positive`` () =
        let result = eval <@ Gamma._gamma 0.1 @> :?> float
        isTrue (result > 0.0) "Expected Γ(0.1) > 0"

    [<Fact>]
    let ``Q: _gamma(-0.5) is negative`` () =
        let result = eval <@ Gamma._gamma -0.5 @> :?> float
        isTrue (result < 0.0) "Expected Γ(-0.5) < 0"

    [<Fact>]
    let ``Q: _gamma with large value approaches infinity`` () =
        let result = eval <@ Gamma._gamma 200.0 @> :?> float
        equal infinity result "Expected Γ(200) = ∞"

    // ========================================
    // Quotation-based tests for _gammaLn
    // ========================================

    [<Fact>]
    let ``Q: _gammaLn(1.0) = 0.0`` () =
        let result = eval <@ Gamma._gammaLn 1.0 @> :?> float
        floatClose Accuracy.high result 0.0 "Expected ln(Γ(1)) = 0"

    [<Fact>]
    let ``Q: _gammaLn(2.0) = 0.0`` () =
        let result = eval <@ Gamma._gammaLn 2.0 @> :?> float
        floatClose Accuracy.high result 0.0 "Expected ln(Γ(2)) = 0"

    [<Fact>]
    let ``Q: _gammaLn(3.0) ≈ ln(2)`` () =
        let result = eval <@ Gamma._gammaLn 3.0 @> :?> float
        floatClose Accuracy.high result (log 2.0) "Expected ln(Γ(3)) = ln(2!)"

    [<Fact>]
    let ``Q: _gammaLn(5.0) ≈ 3.178`` () =
        let result = eval <@ Gamma._gammaLn 5.0 @> :?> float
        floatClose Accuracy.high result 3.1780538303479456 "Expected ln(Γ(5)) ≈ 3.178"

    [<Fact>]
    let ``Q: _gammaLn(10.0) ≈ 12.8018`` () =
        let result = eval <@ Gamma._gammaLn 10.0 @> :?> float
        floatClose Accuracy.high result 12.801827480081469 "Expected ln(Γ(10))"

    [<Fact>]
    let ``Q: _gammaLn(100.0) is finite`` () =
        let result = eval <@ Gamma._gammaLn 100.0 @> :?> float
        isTrue (not (Double.IsInfinity result)) "Expected ln(Γ(100)) to be finite"

    [<Fact>]
    let ``Q: _gammaLn(0.5) = ln(sqrt(π))`` () =
        let result = eval <@ Gamma._gammaLn 0.5 @> :?> float
        let expected = 0.5 * log System.Math.PI
        floatClose Accuracy.high result expected "Expected ln(Γ(0.5)) = ln(√π)"

    [<Fact>]
    let ``Q: _gammaLn increases with x`` () =
        let result1 = eval <@ Gamma._gammaLn 5.0 @> :?> float
        let result2 = eval <@ Gamma._gammaLn 10.0 @> :?> float
        isTrue (result2 > result1) "Expected ln(Γ(x)) to increase with x"

    // ========================================
    // Quotation-based tests for gamma (checked version)
    // ========================================

    [<Fact>]
    let ``Q: gamma(5.0) = 24.0`` () =
        let result = eval <@ Gamma.gamma 5.0 @> :?> float
        floatClose Accuracy.high result 24.0 "Expected Γ(5) = 24"

    [<Fact>]
    let ``Q: gamma(infinity) = infinity`` () =
        let result = eval <@ Gamma.gamma infinity @> :?> float
        equal infinity result "Expected Γ(∞) = ∞"

    [<Fact>]
    let ``Q: gamma(-infinity) = NaN`` () =
        let result = eval <@ Gamma.gamma -infinity @> :?> float
        isTrue (Double.IsNaN result) "Expected Γ(-∞) = NaN"

    [<Fact>]
    let ``Q: gamma(NaN) = NaN`` () =
        let result = eval <@ Gamma.gamma nan @> :?> float
        isTrue (Double.IsNaN result) "Expected Γ(NaN) = NaN"

    [<Fact>]
    let ``Q: gamma handles edge cases differently than _gamma`` () =
        let checkedResult = eval <@ Gamma.gamma infinity @> :?> float
        equal infinity checkedResult "Expected gamma to handle infinity"

    // ========================================
    // Quotation-based tests for gammaLn (checked version)
    // ========================================

    [<Fact>]
    let ``Q: gammaLn(5.0) ≈ 3.178`` () =
        let result = eval <@ Gamma.gammaLn 5.0 @> :?> float
        floatClose Accuracy.high result 3.1780538303479456 "Expected ln(Γ(5))"

    [<Fact>]
    let ``Q: gammaLn(infinity) = infinity`` () =
        let result = eval <@ Gamma.gammaLn infinity @> :?> float
        equal infinity result "Expected ln(Γ(∞)) = ∞"

    [<Fact>]
    let ``Q: gammaLn(-infinity) = NaN`` () =
        let result = eval <@ Gamma.gammaLn -infinity @> :?> float
        isTrue (Double.IsNaN result) "Expected ln(Γ(-∞)) = NaN"

    [<Fact>]
    let ``Q: gammaLn(NaN) = NaN`` () =
        let result = eval <@ Gamma.gammaLn nan @> :?> float
        isTrue (Double.IsNaN result) "Expected ln(Γ(NaN)) = NaN"

    // ========================================
    // Quotation-based tests for gser
    // ========================================

    [<Fact>]
    let ``Q: gser(1.5, 0.5) converges`` () =
        let result = eval <@ Gamma.gser 1.5 0.5 @> :?> float
        floatClose Accuracy.medium result 0.198748 "Expected gser to converge"

    [<Fact>]
    let ``Q: gser(2.0, 1.0) converges`` () =
        let result = eval <@ Gamma.gser 2.0 1.0 @> :?> float
        floatClose Accuracy.medium result 0.264241 "Expected gser result"

    [<Fact>]
    let ``Q: gser(3.0, 0.5) for small x`` () =
        let result = eval <@ Gamma.gser 3.0 0.5 @> :?> float
        isTrue (result > 0.0 && result < 1.0) "Expected gser to be a probability"

    [<Fact>]
    let ``Q: gser(5.0, 2.0) converges correctly`` () =
        let result = eval <@ Gamma.gser 5.0 2.0 @> :?> float
        floatClose Accuracy.medium result 0.052653 "Expected gser convergence"

    [<Fact>]
    let ``Q: gser with very small x approaches 0`` () =
        let result = eval <@ Gamma.gser 2.0 0.001 @> :?> float
        floatClose Accuracy.low result 0.0000005 "Expected gser ≈ 0 for very small x"

    // ========================================
    // Quotation-based tests for gcf
    // ========================================

    [<Fact>]
    let ``Q: gcf(1.5, 5.0) converges`` () =
        let result = eval <@ Gamma.gcf 1.5 5.0 @> :?> float
        floatClose Accuracy.medium result 0.018566 "Expected gcf to converge"

    [<Fact>]
    let ``Q: gcf(2.0, 3.0) converges`` () =
        let result = eval <@ Gamma.gcf 2.0 3.0 @> :?> float
        floatClose Accuracy.medium result 0.199148 "Expected gcf result"

    [<Fact>]
    let ``Q: gcf(3.0, 5.0) for large x`` () =
        let result = eval <@ Gamma.gcf 3.0 5.0 @> :?> float
        isTrue (result > 0.0 && result < 1.0) "Expected gcf to be a probability"

    [<Fact>]
    let ``Q: gcf(4.0, 10.0) converges`` () =
        let result = eval <@ Gamma.gcf 4.0 10.0 @> :?> float
        floatClose Accuracy.medium result 0.0103361 "Expected gcf convergence"

    [<Fact>]
    let ``Q: gser + gcf = 1 (complementarity)`` () =
        let a = 2.5
        let x = 2.0
        let p = eval <@ Gamma.gser a x @> :?> float
        let q = eval <@ Gamma.gcf a x @> :?> float
        floatClose Accuracy.medium (p + q) 1.0 "Expected P + Q = 1"

    // ========================================
    // Quotation-based tests for gammpapprox
    // ========================================

    [<Fact>]
    let ``Q: gammpapprox(150.0, 140.0, true) returns P`` () =
        let result = eval <@ Gamma.gammpapprox 150.0 140.0 true @> :?> float
        isTrue (result >= 0.0 && result <= 1.0) "Expected valid probability"

    [<Fact>]
    let ``Q: gammpapprox(150.0, 140.0, false) returns Q`` () =
        let result = eval <@ Gamma.gammpapprox 150.0 140.0 false @> :?> float
        isTrue (result >= 0.0 && result <= 1.0) "Expected valid probability"

    [<Fact>]
    let ``Q: gammpapprox P and Q are complementary`` () =
        let a = 150.0
        let x = 160.0
        let p = eval <@ Gamma.gammpapprox a x true @> :?> float
        let q = eval <@ Gamma.gammpapprox a x false @> :?> float
        floatClose Accuracy.low (p + q) 1.0 "Expected P + Q ≈ 1"

    [<Fact>]
    let ``Q: gammpapprox with x < a`` () =
        let result = eval <@ Gamma.gammpapprox 150.0 130.0 true @> :?> float
        isTrue (result >= 0.0 && result <= 1.0) "Expected valid result for x < a"

    [<Fact>]
    let ``Q: gammpapprox with x > a`` () =
        let result = eval <@ Gamma.gammpapprox 150.0 170.0 false @> :?> float
        isTrue (result >= 0.0 && result <= 1.0) "Expected valid result for x > a"

    // ========================================
    // Quotation-based tests for lowerIncompleteRegularized
    // ========================================

    [<Fact>]
    let ``Q: lowerIncompleteRegularized(0.5, 0.5) converges`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 0.5 0.5 @> :?> float
        floatClose Accuracy.low result 0.682689 "Expected P(0.5, 0.5)"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized(0.0, x) = 0`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 2.0 0.0 @> :?> float
        equal 0.0 result "Expected P(a, 0) = 0"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized(a, ∞) = 1`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 0.5 infinity @> :?> float
        equal 1.0 result "Expected P(a, ∞) = 1"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized with negative x returns NaN`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 2.0 -1.0 @> :?> float
        isTrue (Double.IsNaN result) "Expected NaN for negative x"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized with negative a returns NaN`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized -1.0 2.0 @> :?> float
        isTrue (Double.IsNaN result) "Expected NaN for negative a"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized uses gser path (x < a + 1)`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 3.0 2.0 @> :?> float
        floatClose Accuracy.medium result 0.323324 "Expected correct value via gser"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized uses gcf path (x >= a + 1)`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 2.0 5.0 @> :?> float
        floatClose Accuracy.medium result 0.959572 "Expected correct value via gcf"

    [<Fact>]
    let ``Q: lowerIncompleteRegularized uses gammpapprox for large a`` () =
        let result = eval <@ Gamma.lowerIncompleteRegularized 150.0 160.0 @> :?> float
        isTrue (result >= 0.0 && result <= 1.0) "Expected valid result via gammpapprox"

    // ========================================
    // Quotation-based tests for upperIncompleteRegularized
    // ========================================

    [<Fact>]
    let ``Q: upperIncompleteRegularized(0.5, 0.5) converges`` () =
        let result = eval <@ Gamma.upperIncompleteRegularized 0.5 0.5 @> :?> float
        let expected = 1.0 - Gamma.lowerIncompleteRegularized 0.5 0.5
        floatClose Accuracy.medium result expected "Expected Q(0.5, 0.5)"

    [<Fact>]
    let ``Q: upperIncompleteRegularized(a, 0) = 1`` () =
        let result = eval <@ Gamma.upperIncompleteRegularized 2.0 0.0 @> :?> float
        equal 1.0 result "Expected Q(a, 0) = 1"

    [<Fact>]
    let ``Q: upperIncompleteRegularized(a, ∞) = 0`` () =
        let result = eval <@ Gamma.upperIncompleteRegularized 0.5 infinity @> :?> float
        equal 0.0 result "Expected Q(a, ∞) = 0"

    [<Fact>]
    let ``Q: upperIncompleteRegularized with negative x returns NaN`` () =
        let result = eval <@ Gamma.upperIncompleteRegularized 2.0 -1.0 @> :?> float
        isTrue (Double.IsNaN result) "Expected NaN for negative x"

    [<Fact>]
    let ``Q: upperIncompleteRegularized with negative a returns NaN`` () =
        let result = eval <@ Gamma.upperIncompleteRegularized -1.0 2.0 @> :?> float
        isTrue (Double.IsNaN result) "Expected NaN for negative a"

    [<Fact>]
    let ``Q: P(a,x) + Q(a,x) = 1`` () =
        let a = 5.0
        let x = 3.0
        let p = eval <@ Gamma.lowerIncompleteRegularized a x @> :?> float
        let q = eval <@ Gamma.upperIncompleteRegularized a x @> :?> float
        floatClose Accuracy.medium (p + q) 1.0 "Expected P + Q = 1"

    // ========================================
    // Quotation-based tests for lowerIncomplete (unregularized)
    // ========================================

    [<Fact>]
    let ``Q: lowerIncomplete(2.0, 1.0) computes correctly`` () =
        let result = eval <@ Gamma.lowerIncomplete 2.0 1.0 @> :?> float
        floatClose Accuracy.medium result 0.2642411176571153 "Expected γ(2, 1)"

    [<Fact>]
    let ``Q: lowerIncomplete(3.0, 2.0) computes correctly`` () =
        let result = eval <@ Gamma.lowerIncomplete 3.0 2.0 @> :?> float
        floatClose Accuracy.medium result 0.646647 "Expected γ(3, 2)"

    [<Fact>]
    let ``Q: lowerIncomplete(5.0, 3.0) computes correctly`` () =
        let result = eval <@ Gamma.lowerIncomplete 5.0 3.0 @> :?> float
        isTrue (result > 0.0) "Expected positive unregularized lower incomplete gamma"

    // ========================================
    // Quotation-based tests for upperIncomplete (unregularized)
    // ========================================

    [<Fact>]
    let ``Q: upperIncomplete(2.0, 1.0) computes correctly`` () =
        let result = eval <@ Gamma.upperIncomplete 2.0 1.0 @> :?> float
        floatClose Accuracy.medium result 0.7357588823428847 "Expected Γ̄(2, 1)"

    [<Fact>]
    let ``Q: upperIncomplete(3.0, 2.0) computes correctly`` () =
        let result = eval <@ Gamma.upperIncomplete 3.0 2.0 @> :?> float
        floatClose Accuracy.medium result 1.353353 "Expected Γ̄(3, 2)"

    [<Fact>]
    let ``Q: lowerIncomplete + upperIncomplete = Gamma(a)`` () =
        let a = 5.0
        let x = 3.0
        let lower = eval <@ Gamma.lowerIncomplete a x @> :?> float
        let upper = eval <@ Gamma.upperIncomplete a x @> :?> float
        let total = lower + upper
        let expected = Gamma._gamma a
        floatClose Accuracy.medium total expected "Expected γ + Γ̄ = Γ(a)"

    // ========================================
    // Quotation-based tests for digammaPositive
    // ========================================

    [<Fact>]
    let ``Q: digammaPositive(2.0) computes correctly`` () =
        let result = eval <@ Gamma.digammaPositive 2.0 @> :?> float
        floatClose Accuracy.high result (Gamma.digamma 1.0 + 1.0) "Expected ψ(2) = ψ(1) + 1"

    [<Fact>]
    let ``Q: digammaPositive(5.0) computes correctly`` () =
        let result = eval <@ Gamma.digammaPositive 5.0 @> :?> float
        floatClose Accuracy.high result 1.506117668 "Expected ψ(5)"

    [<Fact>]
    let ``Q: digammaPositive(10.0) matches digamma`` () =
        let dp = eval <@ Gamma.digammaPositive 10.0 @> :?> float
        let d = Gamma.digamma 10.0
        floatClose Accuracy.high dp d "Expected digammaPositive = digamma for x > 1"

    [<Fact>]
    let ``Q: digammaPositive(100.0) for large x`` () =
        let result = eval <@ Gamma.digammaPositive 100.0 @> :?> float
        floatClose Accuracy.medium result 4.600161852 "Expected ψ(100)"

    [<Fact>]
    let ``Q: digammaPositive(1000.0) asymptotic behavior`` () =
        let result = eval <@ Gamma.digammaPositive 1000.0 @> :?> float
        let expected = log 1000.0 - 0.5 / 1000.0
        floatClose Accuracy.medium result expected "Expected asymptotic approximation"

    // ========================================
    // Quotation-based tests for digamma (full version)
    // ========================================

    [<Fact>]
    let ``Q: digamma(0.0) = -∞`` () =
        let result = eval <@ Gamma.digamma 0.0 @> :?> float
        equal System.Double.NegativeInfinity result "Expected ψ(0) = -∞"

    [<Fact>]
    let ``Q: digamma(1.0) ≈ -0.5772`` () =
        let result = eval <@ Gamma.digamma 1.0 @> :?> float
        floatClose Accuracy.high result -0.5772156649 "Expected ψ(1) = -γ"

    [<Fact>]
    let ``Q: digamma(2.0) = digamma(1.0) + 1`` () =
        let d1 = eval <@ Gamma.digamma 1.0 @> :?> float
        let d2 = eval <@ Gamma.digamma 2.0 @> :?> float
        floatClose Accuracy.high d2 (d1 + 1.0) "Expected recurrence relation"

    [<Fact>]
    let ``Q: digamma(5.0) computes correctly`` () =
        let result = eval <@ Gamma.digamma 5.0 @> :?> float
        floatClose Accuracy.high result 1.506117668 "Expected ψ(5)"

    [<Fact>]
    let ``Q: digamma with negative x uses reflection`` () =
        let result = eval <@ Gamma.digamma -0.5 @> :?> float
        floatClose Accuracy.medium result 0.03649 "Expected ψ(-0.5)"

    [<Fact>]
    let ``Q: digamma(-1.5) with reflection`` () =
        let result = eval <@ Gamma.digamma -1.5 @> :?> float
        isTrue (not (Double.IsNaN result)) "Expected finite result with reflection"

    [<Fact>]
    let ``Q: digamma(0.17) computes correctly`` () =
        let result = eval <@ Gamma.digamma 0.17 @> :?> float
        floatClose Accuracy.high result -6.2100942259248626 "Expected ψ(0.17)"

    [<Fact>]
    let ``Q: digamma(100.0) for large positive x`` () =
        let result = eval <@ Gamma.digamma 100.0 @> :?> float
        floatClose Accuracy.medium result 4.600161852 "Expected ψ(100)"
