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
