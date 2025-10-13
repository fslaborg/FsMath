namespace FsMath.Tests.SpecialFunctionsTests

open Xunit
open System    
open FsMath
open FsMath.SpecialFunctions

open FsMath.Tests.ExpectoStyle



module GammaTests =

    [<Fact>] 
    let ``_gamma(5) = 24`` () = 
        floatClose Accuracy.high (Gamma._gamma 5.) 24. "Should be equal (double precision)"

    [<Fact>] 
    let ``_gamma(-1) = -infinity`` () = 
        equal -infinity (Gamma._gamma -1.) "Expected gamma(-1) = -infinity"

    [<Fact>] 
    let ``_gamma(420) = infinity`` () = 
        equal infinity (Gamma._gamma 420.) "Expected gamma(420) = infinity"

    [<Fact>] 
    let ``_gamma(1) = gamma(1)`` () = 
        equal (Gamma._gamma 1.) (Gamma.gamma 1.) "Expected equal result for checked and unchecked version"

    [<Fact>] 
    let ``gamma(5) = 24`` () = 
        floatClose Accuracy.high (Gamma.gamma 5.) 24. "Should be equal (double precision)"

    [<Fact>] 
    let ``gamma(-1) = -infinity`` () = 
        equal -infinity (Gamma.gamma -1.) "Expected gamma(-1) = -infinity"

    [<Fact>] 
    let ``gamma(420) = infinity`` () = 
        equal infinity (Gamma.gamma 420.) "Expected gamma(420) = infinity"

    [<Fact>] 
    let ``gamma(nan) = nan`` () = 
        isTrue (Double.IsNaN (Gamma.gamma nan)) "Expected gamma(nan) = nan"

    [<Fact>] 
    let ``gamma(infinity) = infinity`` () = 
        equal infinity (Gamma.gamma infinity) "Expected gamma(infinity) = infinity"
    [<Fact>] 
    let ``gamma(-infinity) = nan`` () = 
        isTrue (Double.IsNaN (Gamma.gamma -infinity)) "Expected gamma(-infinity) = nan"

    [<Fact>] 
    let ``_gammaLn(5) ≈ 3.17805...`` () =
        floatClose Accuracy.high (Gamma._gammaLn 5.) 3.1780538303479456 "Should match log gamma"

    [<Fact>] 
    let ``_gammaLn(-1) = nan`` () =
        isTrue (Double.IsNaN (Gamma._gammaLn -1.)) "Expected _gammaLn(-1) = nan"

    [<Fact>] 
    let ``_gammaLn(420) ≈ 2114.8059...`` () =
        floatClose Accuracy.high (Gamma._gammaLn 420.) 2114.8059883267407613276719264808503756320291823875025922347978642 "Should match log gamma for large input"

    [<Fact>] 
    let ``_gammaLn(420) = gammaLn(420)`` () =
        equal (Gamma._gammaLn 420.) (Gamma.gammaLn 420.) "Expected equal result for checked and unchecked gammaLn"

    [<Fact>] 
    let ``gammaLn(5) ≈ 3.17805...`` () =
        floatClose Accuracy.high (Gamma.gammaLn 5.) 3.1780538303479456 "Should match log gamma"

    [<Fact>] 
    let ``gammaLn(-1) = nan`` () =
        isTrue (Double.IsNaN (Gamma.gammaLn -1.)) "Expected gammaLn(-1) = nan"

    [<Fact>] 
    let ``gammaLn(420) ≈ 2114.8059...`` () =
        floatClose Accuracy.high (Gamma.gammaLn 420.) 2114.8059883267407613276719264808503756320291823875025922347978642 "Should match log gamma for large input"

    [<Fact>] 
    let ``gammaLn(nan) = nan`` () =
        isTrue (Double.IsNaN (Gamma.gammaLn nan)) "Expected gammaLn(nan) = nan"

    [<Fact>] 
    let ``gammaLn(infinity) = infinity`` () =
        equal infinity (Gamma.gammaLn infinity) "Expected gammaLn(infinity) = infinity"

    [<Fact>] 
    let ``gammaLn(-infinity) = nan`` () =
        isTrue (Double.IsNaN (Gamma.gammaLn -infinity)) "Expected gammaLn(-infinity) = nan"

    [<Fact>] 
    let ``lowerIncomplete(0.5,0.5) ≈ 0.682689`` () =
        floatClose Accuracy.low (Gamma.lowerIncompleteRegularized 0.5 0.5) 0.682689 "Expected regularized lower incomplete gamma"

    [<Fact>] 
    let ``lowerIncomplete(-1,1) = nan`` () =
        isTrue (Double.IsNaN (Gamma.lowerIncompleteRegularized -1. 1.)) "Expected nan for invalid input"

    [<Fact>] 
    let ``lowerIncomplete(-1,0) = 0`` () =
        floatClose Accuracy.high (Gamma.lowerIncompleteRegularized -1. 0.) 0.0 "Expected lowerIncomplete(-1,0) = 0"

    [<Fact>] 
    let ``lowerIncomplete(0.5, ∞) = 1`` () =
        equal 1.0 (Gamma.lowerIncompleteRegularized 0.5 infinity) "Expected regularized lower incomplete gamma to converge to 1"

    [<Fact>] 
    let ``upperIncomplete(0.5,0.5) = 1 - lowerIncomplete(0.5,0.5)`` () =
        let expected = 1. - Gamma.lowerIncompleteRegularized 0.5 0.5
        let actual = Gamma.upperIncompleteRegularized 0.5 0.5
        floatClose Accuracy.medium actual expected "Expected complementary incomplete gamma"

    [<Fact>] 
    let ``upperIncomplete(-1,1) = nan`` () =
        isTrue (Double.IsNaN (Gamma.upperIncompleteRegularized -1. 1.)) "Expected nan for invalid input"

    [<Fact>]
    let ``upperIncomplete(0.5, ∞) = 0`` () =
        equal 0.0 (Gamma.upperIncompleteRegularized 0.5 infinity) "Expected upper incomplete gamma to vanish at ∞"

    [<Fact>] 
    let ``digamma(0.17) ≈ -6.21009...`` () =
        floatClose Accuracy.high (Gamma.digamma 0.17) -6.2100942259248626 "Expected digamma at x = 0.17"

    [<Fact>] 
    let ``digamma(-1.82096...) ≈ -4.13430...`` () =
        floatClose Accuracy.high (Gamma.digamma -1.8209678549077879) -4.1343001655848468 "Expected digamma at negative x"

    //[<Fact>] 
    //let ``trigamma(0.17) ≈ 35.9153...`` () =
    //    floatClose Accuracy.high (Gamma.trigamma 0.17) 35.915302055854525 "Expected trigamma at x = 0.17"

    //[<Fact>] 
    //let ``trigamma(-1.82096...) ≈ 34.2831...`` () =
    //    floatClose Accuracy.high (Gamma.trigamma -1.8209678549077879) 34.283184056369407 "Expected trigamma at negative x"





    //[<Fact>]
    //let ``_gamma(5)`` () =
    //    let gam = Gamma._gamma 5.
    //    floatClose 24. gam 1e-12

    //[<Fact>]
    //let ``_gamma(-1)`` () =
    //    let gam = Gamma._gamma -1.
    //    Assert.Equal(-infinity, gam)

    //[<Fact>]
    //let ``_gamma(420) returns infinity`` () =
    //    let gam = Gamma._gamma 420.
    //    Assert.Equal(infinity, gam)

    //[<Fact>]
    //let ``_gamma(1) = gamma(1)`` () =
    //    let _gam = Gamma._gamma 1.
    //    let gam = Gamma.gamma 1.
    //    Assert.Equal(_gam, gam)

    //[<Fact>]
    //let ``gamma(5)`` () =
    //    let gam = Gamma.gamma 5.
    //    floatClose 24. gam 1e-12

    //[<Fact>]
    //let ``gamma(-1)`` () =
    //    let gam = Gamma.gamma -1.
    //    Assert.Equal(-infinity, gam)

    //[<Fact>]
    //let ``gamma(420) returns infinity`` () =
    //    let gam = Gamma.gamma 420.
    //    Assert.Equal(infinity, gam)

    //[<Fact>]
    //let ``gamma(nan) = nan`` () =
    //    let gam = Gamma.gamma nan
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``gamma(infinity) = infinity`` () =
    //    let gam = Gamma.gamma infinity
    //    Assert.Equal(infinity, gam)

    //[<Fact>]
    //let ``gamma(-infinity) = nan`` () =
    //    let gam = Gamma.gamma -infinity
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``_gammaLn(5)`` () =
    //    let gam = Gamma._gammaLn 5.
    //    floatClose 3.1780538303479456 gam 1e-12

    //[<Fact>]
    //let ``_gammaLn(-1)`` () =
    //    let gam = Gamma._gammaLn -1.
    //    Assert.True(Double.IsNaN gam)

    //// [<Fact>]
    //// let ``_gammaLn(420)`` () =
    ////     let gam = Gamma._gammaLn 420.
    ////     floatClose 2114.8059883267407 gam 1e-10


    //[<Fact>]
    //let ``_gammaLn(420)`` () =
    //    let gam = Gamma._gammaLn 420.
    //    floatClose  Accuracy.high gam 2114.8059883267407613276719264808503756320291823875025922347978642 "Should be equal (double precision)"


    //[<Fact>]
    //let ``_gammaLn(420) = gammaLn(420)`` () =
    //    let _gam = Gamma._gammaLn 420.
    //    let gam = Gamma.gammaLn 420.
    //    Assert.Equal(_gam, gam)

    //[<Fact>]
    //let ``gammaLn(5)`` () =
    //    let gam = Gamma.gammaLn 5.
    //    floatClose 3.1780538303479456 gam 1e-12

    //[<Fact>]
    //let ``gammaLn(-1)`` () =
    //    let gam = Gamma.gammaLn -1.
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``gammaLn(420)`` () =
    //    let gam = Gamma.gammaLn 420.
    //    ExpectoStyle.floatClose
    //        Accuracy.high gam 2114.8059883267407613276719264808503756320291823875025922347978642 "Should be equal (double precision)"

    //[<Fact>]
    //let ``gammaLn(nan) = nan`` () =
    //    let gam = Gamma.gammaLn nan
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``gammaLn(infinity) = infinity`` () =
    //    let gam = Gamma.gammaLn infinity
    //    Assert.Equal(infinity, gam)

    //[<Fact>]
    //let ``gammaLn(-infinity) = nan`` () =
    //    let gam = Gamma.gammaLn -infinity
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``lowerIncomplete(0.5, 0.5)`` () =
    //    let gam = Gamma.lowerIncompleteRegularized 0.5 0.5
    //    floatClose 0.682689 gam 1e-5

    //[<Fact>]
    //let ``lowerIncomplete(-1, 1) = nan`` () =
    //    let gam = Gamma.lowerIncompleteRegularized -1. 1.
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``lowerIncomplete(-1, 0) = 0`` () =
    //    let gam = Gamma.lowerIncompleteRegularized -1. 0.
    //    floatClose 0.0 gam 1e-12

    //[<Fact>]
    //let ``lowerIncomplete(0.5, infinity) = 1`` () =
    //    let gam = Gamma.lowerIncompleteRegularized 0.5 infinity
    //    Assert.Equal(1.0, gam)

    //[<Fact>]
    //let ``upperIncomplete(0.5, 0.5)`` () =
    //    let gamu = Gamma.upperIncompleteRegularized 0.5 0.5
    //    let gam  = 1. - Gamma.lowerIncompleteRegularized 0.5 0.5
    //    floatClose gam gamu 1e-6

    //[<Fact>]
    //let ``upperIncomplete(-1,1) = nan`` () =
    //    let gam = Gamma.upperIncompleteRegularized -1. 1.
    //    Assert.True(Double.IsNaN gam)

    //[<Fact>]
    //let ``upperIncomplete(0.5, infinity) = 0`` () =
    //    let gam = Gamma.upperIncompleteRegularized 0.5 infinity
    //    Assert.Equal(0.0, gam)

    //[<Fact>]
    //let ``digamma(0.17)`` () =
    //    let diGam = Gamma.digamma 0.17
    //    floatClose -6.2100942259248626 diGam 1e-12

    //[<Fact>]
    //let ``digamma(-1.8209678549077879)`` () =
    //    let diGam = Gamma.digamma -1.8209678549077879
    //    floatClose -4.1343001655848468 diGam 1e-12

    //// [<Fact>]
    //// let ``trigamma(0.17)`` () =
    ////     let triGam = Gamma.trigamma 0.17
    ////     floatClose 35.915302055854525 triGam 1e-12

    //// [<Fact>]
    //// let ``trigamma(-1.8209678549077879)`` () =
    ////     let triGam = Gamma.trigamma -1.8209678549077879
    ////     floatClose 34.283184056369407 triGam 1e-12


// Additional comprehensive tests for Gamma functions
module GammaComprehensiveTests =

    // Tests for unregularized incomplete gamma functions
    [<Fact>]
    let ``lowerIncomplete(2.0, 1.0) computes unregularized lower incomplete gamma`` () =
        let result = Gamma.lowerIncomplete 2.0 1.0
        // γ(2, 1) = Γ(2) * P(2, 1) = 1! * P(2, 1) ≈ 0.2642411176571153
        floatClose Accuracy.medium result 0.2642411176571153 "Expected unregularized lower incomplete gamma"

    [<Fact>]
    let ``lowerIncomplete(3.0, 2.0) computes correct value`` () =
        let result = Gamma.lowerIncomplete 3.0 2.0
        // γ(3, 2) = Γ(3) * P(3, 2) = 2! * P(3, 2) ≈ 0.646647
        floatClose Accuracy.medium result 0.646647 "Expected unregularized lower incomplete gamma"

    [<Fact>]
    let ``upperIncomplete(2.0, 1.0) computes unregularized upper incomplete gamma`` () =
        let result = Gamma.upperIncomplete 2.0 1.0
        // Γ̄(2, 1) = Γ(2) * Q(2, 1) = 1! * Q(2, 1) ≈ 0.7357588823428847
        floatClose Accuracy.medium result 0.7357588823428847 "Expected unregularized upper incomplete gamma"

    [<Fact>]
    let ``upperIncomplete(3.0, 2.0) computes correct value`` () =
        let result = Gamma.upperIncomplete 3.0 2.0
        // Γ̄(3, 2) = Γ(3) * Q(3, 2) = 2! * Q(3, 2) ≈ 1.353353
        floatClose Accuracy.medium result 1.353353 "Expected unregularized upper incomplete gamma"

    [<Fact>]
    let ``lowerIncomplete + upperIncomplete = Gamma(a)`` () =
        let a = 5.0
        let x = 3.0
        let lower = Gamma.lowerIncomplete a x
        let upper = Gamma.upperIncomplete a x
        let total = lower + upper
        let expected = Gamma._gamma a
        floatClose Accuracy.medium total expected "Lower + upper incomplete should equal Γ(a)"

    // Tests for gser (series representation of lower incomplete gamma)
    [<Fact>]
    let ``gser(1.5, 0.5) converges to correct value`` () =
        let result = Gamma.gser 1.5 0.5
        floatClose Accuracy.medium result 0.198748 "Expected gser convergence"

    [<Fact>]
    let ``gser(2.0, 1.0) converges for small x`` () =
        let result = Gamma.gser 2.0 1.0
        floatClose Accuracy.medium result 0.264241 "Expected gser to converge"

    [<Fact>]
    let ``gser with very small x approaches 0`` () =
        let result = Gamma.gser 2.0 0.001
        floatClose Accuracy.low result 0.0000005 "Expected gser to be close to 0 for very small x"

    // Tests for gcf (continued fraction representation of upper incomplete gamma)
    [<Fact>]
    let ``gcf(1.5, 5.0) converges to correct value`` () =
        let result = Gamma.gcf 1.5 5.0
        floatClose Accuracy.medium result 0.018566 "Expected gcf convergence"

    [<Fact>]
    let ``gcf(2.0, 3.0) converges for large x`` () =
        let result = Gamma.gcf 2.0 3.0
        floatClose Accuracy.medium result 0.199148 "Expected gcf to converge"

    [<Fact>]
    let ``gser + gcf = 1 (P + Q = 1)`` () =
        let a = 2.5
        let x = 2.0
        let p = Gamma.gser a x
        let q = Gamma.gcf a x
        floatClose Accuracy.medium (p + q) 1.0 "Expected P(a,x) + Q(a,x) = 1"

    // Tests for gammpapprox (Gauss-Legendre quadrature)
    [<Fact>]
    let ``gammpapprox(150.0, 140.0, true) computes P approximation`` () =
        let result = Gamma.gammpapprox 150.0 140.0 true
        isTrue (result >= 0.0 && result <= 1.0) "Expected gammpapprox to return valid probability"

    [<Fact>]
    let ``gammpapprox(150.0, 140.0, false) computes Q approximation`` () =
        let result = Gamma.gammpapprox 150.0 140.0 false
        isTrue (result >= 0.0 && result <= 1.0) "Expected gammpapprox to return valid probability"

    [<Fact>]
    let ``gammpapprox P and Q are complementary`` () =
        let a = 150.0
        let x = 160.0
        let p = Gamma.gammpapprox a x true
        let q = Gamma.gammpapprox a x false
        floatClose Accuracy.low (p + q) 1.0 "Expected P + Q ≈ 1 for gammpapprox"

    // Tests for lowerIncompleteRegularized edge cases and different code paths
    [<Fact>]
    let ``lowerIncompleteRegularized uses gser when x < a + 1`` () =
        let result = Gamma.lowerIncompleteRegularized 3.0 2.0 // x < a + 1
        floatClose Accuracy.medium result 0.323324 "Expected correct value via gser path"

    [<Fact>]
    let ``lowerIncompleteRegularized uses gcf when x >= a + 1`` () =
        let result = Gamma.lowerIncompleteRegularized 2.0 5.0 // x >= a + 1
        floatClose Accuracy.medium result 0.959572 "Expected correct value via gcf path"

    [<Fact>]
    let ``lowerIncompleteRegularized uses gammpapprox for large a`` () =
        let result = Gamma.lowerIncompleteRegularized 150.0 160.0 // a >= 100
        isTrue (result >= 0.0 && result <= 1.0) "Expected valid probability via gammpapprox"

    [<Fact>]
    let ``upperIncompleteRegularized uses different code paths`` () =
        // Test x < a + 1 path
        let result1 = Gamma.upperIncompleteRegularized 3.0 2.0
        floatClose Accuracy.medium result1 0.676676 "Expected correct value via gser path"

        // Test x >= a + 1 path
        let result2 = Gamma.upperIncompleteRegularized 2.0 5.0
        floatClose Accuracy.medium result2 0.040428 "Expected correct value via gcf path"

    [<Fact>]
    let ``upperIncompleteRegularized(0.5, 0) = 1`` () =
        equal 1.0 (Gamma.upperIncompleteRegularized 0.5 0.0) "Expected Q(a, 0) = 1"

    [<Fact>]
    let ``lowerIncompleteRegularized with x < 0 returns nan`` () =
        isTrue (Double.IsNaN (Gamma.lowerIncompleteRegularized 2.0 -1.0)) "Expected nan for negative x"

    [<Fact>]
    let ``upperIncompleteRegularized with x < 0 returns nan`` () =
        isTrue (Double.IsNaN (Gamma.upperIncompleteRegularized 2.0 -1.0)) "Expected nan for negative x"

    // Tests for digamma function
    [<Fact>]
    let ``digamma(1.0) ≈ -0.5772156649`` () =
        floatClose Accuracy.high (Gamma.digamma 1.0) -0.5772156649 "Expected digamma(1) = -γ (negative Euler-Mascheroni constant)"

    [<Fact>]
    let ``digamma(2.0) = digamma(1.0) + 1`` () =
        let d1 = Gamma.digamma 1.0
        let d2 = Gamma.digamma 2.0
        floatClose Accuracy.high d2 (d1 + 1.0) "Expected digamma recurrence relation"

    [<Fact>]
    let ``digamma(0) = -infinity`` () =
        equal System.Double.NegativeInfinity (Gamma.digamma 0.0) "Expected digamma(0) = -∞"

    [<Fact>]
    let ``digamma with large positive x`` () =
        let result = Gamma.digamma 100.0
        floatClose Accuracy.medium result 4.600161852 "Expected digamma to converge for large x"

    [<Fact>]
    let ``digamma with negative x uses reflection`` () =
        let result = Gamma.digamma -0.5
        // ψ(-0.5) ≈ 0.03648997397857652
        floatClose Accuracy.medium result 0.03649 "Expected digamma with reflection formula"

    [<Fact>]
    let ``digammaPositive(5.0) computes correctly`` () =
        let result = Gamma.digammaPositive 5.0
        floatClose Accuracy.high result 1.506117668 "Expected digammaPositive to match digamma for x > 1"

    [<Fact>]
    let ``digammaPositive(10.0) matches digamma(10.0)`` () =
        let dp = Gamma.digammaPositive 10.0
        let d = Gamma.digamma 10.0
        floatClose Accuracy.high dp d "Expected digammaPositive to equal digamma for positive x"

    [<Fact>]
    let ``digammaPositive with very large x`` () =
        let result = Gamma.digammaPositive 1000.0
        let expected = log 1000.0 - 0.5 / 1000.0 // Asymptotic approximation
        floatClose Accuracy.medium result expected "Expected digammaPositive asymptotic behavior"

    // Additional edge cases for gamma and gammaLn
    [<Fact>]
    let ``gamma(0.5) = sqrt(π)`` () =
        floatClose Accuracy.high (Gamma.gamma 0.5) (sqrt System.Math.PI) "Expected gamma(1/2) = √π"

    [<Fact>]
    let ``gamma(3.5) matches expected value`` () =
        // Γ(3.5) = 2.5 * 1.5 * 0.5 * Γ(0.5) = 2.5 * 1.5 * 0.5 * √π
        let expected = 2.5 * 1.5 * 0.5 * sqrt System.Math.PI
        floatClose Accuracy.high (Gamma.gamma 3.5) expected "Expected gamma(3.5) via recurrence"

    [<Fact>]
    let ``gammaLn(1.0) = 0`` () =
        floatClose Accuracy.high (Gamma.gammaLn 1.0) 0.0 "Expected ln(Γ(1)) = ln(1) = 0"

    [<Fact>]
    let ``gammaLn(2.0) = 0`` () =
        floatClose Accuracy.high (Gamma.gammaLn 2.0) 0.0 "Expected ln(Γ(2)) = ln(1) = 0"

    [<Fact>]
    let ``gammaLn matches log of gamma for small values`` () =
        let x = 3.0
        let gln = Gamma.gammaLn x
        let g = Gamma.gamma x
        floatClose Accuracy.high gln (log g) "Expected gammaLn = log(gamma)"

    // Consistency tests
    [<Fact>]
    let ``P(a,x) + Q(a,x) = 1 for various inputs`` () =
        let testCases = [(1.0, 1.0); (2.5, 3.0); (5.0, 4.0); (10.0, 12.0)]
        for (a, x) in testCases do
            let p = Gamma.lowerIncompleteRegularized a x
            let q = Gamma.upperIncompleteRegularized a x
            floatClose Accuracy.medium (p + q) 1.0 $"Expected P + Q = 1 for a={a}, x={x}"

    [<Fact>]
    let ``gamma maximum constant is approximately 171.624`` () =
        floatClose Accuracy.high Gamma.maximum 171.624376956302725 "Expected gamma maximum constant"

    [<Fact>]
    let ``gamma(maximum) is near overflow threshold`` () =
        let result = Gamma.gamma Gamma.maximum
        // gamma(171.624) is very large but shouldn't be exactly infinity
        isTrue (result > 1e100) "Expected gamma(maximum) to be very large"
