namespace FsMath.Tests.GenericMath

open System
open Xunit
open FsMath
open FsMath.GenericMath
open FsMath.Tests.AssertHelpers

module GenericMathTests =

    [<Fact>]
    let ``zero: returns correct zero for float`` () =
        let result = zero<float>
        Assert.Equal(0.0, result)

    [<Fact>]
    let ``zero: returns correct zero for int`` () =
        let result = zero<int>
        Assert.Equal(0, result)

    [<Fact>]
    let ``one: returns correct one for float`` () =
        let result = one<float>
        Assert.Equal(1.0, result)

    [<Fact>]
    let ``one: returns correct one for int`` () =
        let result = one<int>
        Assert.Equal(1, result)

    [<Fact>]
    let ``T: converts float to float`` () =
        let result = T<float> 3.14
        floatEqual 3.14 result 1e-10

    [<Fact>]
    let ``T: converts float to int`` () =
        let result = T<int> 42.0
        Assert.Equal(42, result)

    [<Fact>]
    let ``sqrt: computes square root of float`` () =
        let result = sqrt<float> 16.0
        floatEqual 4.0 result 1e-10

    [<Fact>]
    let ``sqrt: computes square root of 2.0`` () =
        let result = sqrt<float> 2.0
        floatEqual 1.4142135623730951 result 1e-10

    [<Fact>]
    let ``exp: computes e^x for float`` () =
        let result = exp<float> 1.0
        floatEqual Math.E result 1e-10

    [<Fact>]
    let ``exp: computes e^0`` () =
        let result = exp<float> 0.0
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``exp: computes e^2`` () =
        let result = exp<float> 2.0
        floatEqual 7.38905609893065 result 1e-10

    [<Fact>]
    let ``pow: computes 2^3`` () =
        let result = pow<float> 2.0 3.0
        floatEqual 8.0 result 1e-10

    [<Fact>]
    let ``pow: computes 10^2`` () =
        let result = pow<float> 10.0 2.0
        floatEqual 100.0 result 1e-10

    [<Fact>]
    let ``pow: computes 5^0`` () =
        let result = pow<float> 5.0 0.0
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``log: computes natural log of e`` () =
        let result = log<float> Math.E
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``log: computes natural log of 1`` () =
        let result = log<float> 1.0
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``log: computes natural log of 10`` () =
        let result = log<float> 10.0
        floatEqual 2.302585092994046 result 1e-10

    [<Fact>]
    let ``abs: returns absolute value of positive number`` () =
        let result = abs<float> 5.5
        floatEqual 5.5 result 1e-10

    [<Fact>]
    let ``abs: returns absolute value of negative number`` () =
        let result = abs<float> -5.5
        floatEqual 5.5 result 1e-10

    [<Fact>]
    let ``abs: returns zero for zero`` () =
        let result = abs<float> 0.0
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``abs: works with integers`` () =
        let result = abs<int> -42
        Assert.Equal(42, result)

    [<Fact>]
    let ``sin: computes sine of 0`` () =
        let result = sin<float> 0.0
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``sin: computes sine of pi/2`` () =
        let result = sin<float> (Math.PI / 2.0)
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``sin: computes sine of pi`` () =
        let result = sin<float> Math.PI
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``cos: computes cosine of 0`` () =
        let result = cos<float> 0.0
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``cos: computes cosine of pi`` () =
        let result = cos<float> Math.PI
        floatEqual -1.0 result 1e-10

    [<Fact>]
    let ``cos: computes cosine of pi/2`` () =
        let result = cos<float> (Math.PI / 2.0)
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``pi: returns correct value for float`` () =
        let result = pi<float> ()
        floatEqual Math.PI result 1e-10

    [<Fact>]
    let ``e: returns Euler's number for float`` () =
        let result = e<float> ()
        floatEqual Math.E result 1e-10

    [<Fact>]
    let ``tau: returns 2*pi for float`` () =
        let result = tau<float> ()
        floatEqual (2.0 * Math.PI) result 1e-10

    [<Fact>]
    let ``floor: rounds down positive number`` () =
        let result = floor<float> 3.7
        floatEqual 3.0 result 1e-10

    [<Fact>]
    let ``floor: rounds down negative number`` () =
        let result = floor<float> -2.3
        floatEqual -3.0 result 1e-10

    [<Fact>]
    let ``floor: keeps integer unchanged`` () =
        let result = floor<float> 5.0
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``epsilon: returns a very small positive value`` () =
        let result = epsilon<float> ()
        Assert.True(result > 0.0)
        Assert.True(result < 1e-300)
