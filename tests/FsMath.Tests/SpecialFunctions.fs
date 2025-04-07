module SpecialFunctionsTests

open Xunit
open System    
open FsMath
open FsMath.SpecialFunctions




module GammaTests =

    let inline floatClose (expected: float) (actual: float) (tolerance: float) =
        Assert.InRange(actual, expected - tolerance, expected + tolerance)

    [<Fact>]
    let ``_gamma(5)`` () =
        let gam = Gamma._gamma 5.
        floatClose 24. gam 1e-12

//     [<Fact>]
//     let ``_gamma(-1)`` () =
//         let gam = Gamma._gamma -1.
//         Assert.Equal(-infinity, gam)

//     [<Fact>]
//     let ``_gamma(420) returns infinity`` () =
//         let gam = Gamma._gamma 420.
//         Assert.Equal(infinity, gam)

//     [<Fact>]
//     let ``_gamma(1) = gamma(1)`` () =
//         let _gam = Gamma._gamma 1.
//         let gam = Gamma.gamma 1.
//         Assert.Equal(_gam, gam)

//     [<Fact>]
//     let ``gamma(5)`` () =
//         let gam = Gamma.gamma 5.
//         floatClose 24. gam 1e-12

//     [<Fact>]
//     let ``gamma(-1)`` () =
//         let gam = Gamma.gamma -1.
//         Assert.Equal(-infinity, gam)

//     [<Fact>]
//     let ``gamma(420) returns infinity`` () =
//         let gam = Gamma.gamma 420.
//         Assert.Equal(infinity, gam)

//     [<Fact>]
//     let ``gamma(nan) = nan`` () =
//         let gam = Gamma.gamma nan
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``gamma(infinity) = infinity`` () =
//         let gam = Gamma.gamma infinity
//         Assert.Equal(infinity, gam)

//     [<Fact>]
//     let ``gamma(-infinity) = nan`` () =
//         let gam = Gamma.gamma -infinity
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``_gammaLn(5)`` () =
//         let gam = Gamma._gammaLn 5.
//         floatClose 3.1780538303479456 gam 1e-12

//     [<Fact>]
//     let ``_gammaLn(-1)`` () =
//         let gam = Gamma._gammaLn -1.
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``_gammaLn(420)`` () =
//         let gam = Gamma._gammaLn 420.
//         floatClose 2114.8059883267407 gam 1e-10

//     [<Fact>]
//     let ``_gammaLn(420) = gammaLn(420)`` () =
//         let _gam = Gamma._gammaLn 420.
//         let gam = Gamma.gammaLn 420.
//         Assert.Equal(_gam, gam)

//     [<Fact>]
//     let ``gammaLn(5)`` () =
//         let gam = Gamma.gammaLn 5.
//         floatClose 3.1780538303479456 gam 1e-12

//     [<Fact>]
//     let ``gammaLn(-1)`` () =
//         let gam = Gamma.gammaLn -1.
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``gammaLn(420)`` () =
//         let gam = Gamma.gammaLn 420.
//         floatClose 2114.8059883267407 gam 1e-10

//     [<Fact>]
//     let ``gammaLn(nan) = nan`` () =
//         let gam = Gamma.gammaLn nan
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``gammaLn(infinity) = infinity`` () =
//         let gam = Gamma.gammaLn infinity
//         Assert.Equal(infinity, gam)

//     [<Fact>]
//     let ``gammaLn(-infinity) = nan`` () =
//         let gam = Gamma.gammaLn -infinity
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``lowerIncomplete(0.5, 0.5)`` () =
//         let gam = Gamma.lowerIncompleteRegularized 0.5 0.5
//         floatClose 0.682689 gam 1e-5

//     [<Fact>]
//     let ``lowerIncomplete(-1, 1) = nan`` () =
//         let gam = Gamma.lowerIncompleteRegularized -1. 1.
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``lowerIncomplete(-1, 0) = 0`` () =
//         let gam = Gamma.lowerIncompleteRegularized -1. 0.
//         floatClose 0.0 gam 1e-12

//     [<Fact>]
//     let ``lowerIncomplete(0.5, infinity) = 1`` () =
//         let gam = Gamma.lowerIncompleteRegularized 0.5 infinity
//         Assert.Equal(1.0, gam)

//     [<Fact>]
//     let ``upperIncomplete(0.5, 0.5)`` () =
//         let gamu = Gamma.upperIncompleteRegularized 0.5 0.5
//         let gam  = 1. - Gamma.lowerIncompleteRegularized 0.5 0.5
//         floatClose gam gamu 1e-6

//     [<Fact>]
//     let ``upperIncomplete(-1,1) = nan`` () =
//         let gam = Gamma.upperIncompleteRegularized -1. 1.
//         Assert.True(Double.IsNaN gam)

//     [<Fact>]
//     let ``upperIncomplete(0.5, infinity) = 0`` () =
//         let gam = Gamma.upperIncompleteRegularized 0.5 infinity
//         Assert.Equal(0.0, gam)

//     [<Fact>]
//     let ``digamma(0.17)`` () =
//         let diGam = Gamma.digamma 0.17
//         floatClose -6.2100942259248626 diGam 1e-12

//     [<Fact>]
//     let ``digamma(-1.8209678549077879)`` () =
//         let diGam = Gamma.digamma -1.8209678549077879
//         floatClose -4.1343001655848468 diGam 1e-12

//     [<Fact>]
//     let ``trigamma(0.17)`` () =
//         let triGam = Gamma.trigamma 0.17
//         floatClose 35.915302055854525 triGam 1e-12

//     [<Fact>]
//     let ``trigamma(-1.8209678549077879)`` () =
//         let triGam = Gamma.trigamma -1.8209678549077879
//         floatClose 34.283184056369407 triGam 1e-12
