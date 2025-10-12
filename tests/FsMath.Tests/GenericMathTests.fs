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

// ========================================
// Quotation-based tests for inline functions
// ========================================
// These tests use F# quotation evaluation to force inline functions to be tracked by coverage tools.
// The quotation evaluation technique dynamically invokes the inline functions, allowing coverage
// analysis to properly instrument them.

module GenericMathQuotationTests =
    open FSharp.Linq.RuntimeHelpers

    let inline eval q = LeafExpressionConverter.EvaluateQuotation q

    [<Fact>]
    let ``Q: zero returns correct zero for float`` () =
        let result = eval <@ zero<float> @> :?> float
        Assert.Equal(0.0, result)

    [<Fact>]
    let ``Q: zero returns correct zero for int`` () =
        let result = eval <@ zero<int> @> :?> int
        Assert.Equal(0, result)

    [<Fact>]
    let ``Q: one returns correct one for float`` () =
        let result = eval <@ one<float> @> :?> float
        Assert.Equal(1.0, result)

    [<Fact>]
    let ``Q: one returns correct one for int`` () =
        let result = eval <@ one<int> @> :?> int
        Assert.Equal(1, result)

    [<Fact>]
    let ``Q: T converts float to float`` () =
        let result = eval <@ T<float> 3.14 @> :?> float
        floatEqual 3.14 result 1e-10

    [<Fact>]
    let ``Q: T converts float to int`` () =
        let result = eval <@ T<int> 42.0 @> :?> int
        Assert.Equal(42, result)

    [<Fact>]
    let ``Q: T handles negative conversion`` () =
        let result = eval <@ T<int> -25.0 @> :?> int
        Assert.Equal(-25, result)

    [<Fact>]
    let ``Q: T handles zero conversion`` () =
        let result = eval <@ T<float> 0.0 @> :?> float
        Assert.Equal(0.0, result)

    [<Fact>]
    let ``Q: sqrt computes square root of float`` () =
        let result = eval <@ sqrt<float> 16.0 @> :?> float
        floatEqual 4.0 result 1e-10

    [<Fact>]
    let ``Q: sqrt computes square root of 2.0`` () =
        let result = eval <@ sqrt<float> 2.0 @> :?> float
        floatEqual 1.4142135623730951 result 1e-10

    [<Fact>]
    let ``Q: sqrt of 0 returns 0`` () =
        let result = eval <@ sqrt<float> 0.0 @> :?> float
        Assert.Equal(0.0, result)

    [<Fact>]
    let ``Q: sqrt of 1 returns 1`` () =
        let result = eval <@ sqrt<float> 1.0 @> :?> float
        Assert.Equal(1.0, result)

    [<Fact>]
    let ``Q: exp computes e^x for float`` () =
        let result = eval <@ exp<float> 1.0 @> :?> float
        floatEqual Math.E result 1e-10

    [<Fact>]
    let ``Q: exp computes e^0`` () =
        let result = eval <@ exp<float> 0.0 @> :?> float
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``Q: exp computes e^2`` () =
        let result = eval <@ exp<float> 2.0 @> :?> float
        floatEqual 7.38905609893065 result 1e-10

    [<Fact>]
    let ``Q: exp of negative value`` () =
        let result = eval <@ exp<float> -1.0 @> :?> float
        floatEqual 0.36787944117144233 result 1e-10

    [<Fact>]
    let ``Q: pow computes 2^3`` () =
        let result = eval <@ pow<float> 2.0 3.0 @> :?> float
        floatEqual 8.0 result 1e-10

    [<Fact>]
    let ``Q: pow computes 10^2`` () =
        let result = eval <@ pow<float> 10.0 2.0 @> :?> float
        floatEqual 100.0 result 1e-10

    [<Fact>]
    let ``Q: pow computes 5^0`` () =
        let result = eval <@ pow<float> 5.0 0.0 @> :?> float
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``Q: pow with negative exponent`` () =
        let result = eval <@ pow<float> 2.0 -2.0 @> :?> float
        floatEqual 0.25 result 1e-10

    [<Fact>]
    let ``Q: pow with fractional exponent`` () =
        let result = eval <@ pow<float> 4.0 0.5 @> :?> float
        floatEqual 2.0 result 1e-10

    [<Fact>]
    let ``Q: log computes natural log of e`` () =
        let result = eval <@ log<float> Math.E @> :?> float
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``Q: log computes natural log of 1`` () =
        let result = eval <@ log<float> 1.0 @> :?> float
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``Q: log computes natural log of 10`` () =
        let result = eval <@ log<float> 10.0 @> :?> float
        floatEqual 2.302585092994046 result 1e-10

    [<Fact>]
    let ``Q: log of e^2`` () =
        let result = eval <@ log<float> (Math.E * Math.E) @> :?> float
        floatEqual 2.0 result 1e-10

    [<Fact>]
    let ``Q: abs returns absolute value of positive number`` () =
        let result = eval <@ abs<float> 5.5 @> :?> float
        floatEqual 5.5 result 1e-10

    [<Fact>]
    let ``Q: abs returns absolute value of negative number`` () =
        let result = eval <@ abs<float> -5.5 @> :?> float
        floatEqual 5.5 result 1e-10

    [<Fact>]
    let ``Q: abs returns zero for zero`` () =
        let result = eval <@ abs<float> 0.0 @> :?> float
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``Q: abs works with integers`` () =
        let result = eval <@ abs<int> -42 @> :?> int
        Assert.Equal(42, result)

    [<Fact>]
    let ``Q: abs of large negative int`` () =
        let result = eval <@ abs<int> -12345 @> :?> int
        Assert.Equal(12345, result)

    [<Fact>]
    let ``Q: sin computes sine of 0`` () =
        let result = eval <@ sin<float> 0.0 @> :?> float
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``Q: sin computes sine of pi/2`` () =
        let result = eval <@ sin<float> (Math.PI / 2.0) @> :?> float
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``Q: sin computes sine of pi`` () =
        let result = eval <@ sin<float> Math.PI @> :?> float
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``Q: sin computes sine of pi/6`` () =
        let result = eval <@ sin<float> (Math.PI / 6.0) @> :?> float
        floatEqual 0.5 result 1e-10

    [<Fact>]
    let ``Q: cos computes cosine of 0`` () =
        let result = eval <@ cos<float> 0.0 @> :?> float
        floatEqual 1.0 result 1e-10

    [<Fact>]
    let ``Q: cos computes cosine of pi`` () =
        let result = eval <@ cos<float> Math.PI @> :?> float
        floatEqual -1.0 result 1e-10

    [<Fact>]
    let ``Q: cos computes cosine of pi/2`` () =
        let result = eval <@ cos<float> (Math.PI / 2.0) @> :?> float
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``Q: cos computes cosine of pi/3`` () =
        let result = eval <@ cos<float> (Math.PI / 3.0) @> :?> float
        floatEqual 0.5 result 1e-10

    [<Fact>]
    let ``Q: pi returns correct value for float`` () =
        let result = eval <@ pi<float> () @> :?> float
        floatEqual Math.PI result 1e-10

    [<Fact>]
    let ``Q: e returns Euler's number for float`` () =
        let result = eval <@ e<float> () @> :?> float
        floatEqual Math.E result 1e-10

    [<Fact>]
    let ``Q: tau returns 2*pi for float`` () =
        let result = eval <@ tau<float> () @> :?> float
        floatEqual (2.0 * Math.PI) result 1e-10

    [<Fact>]
    let ``Q: floor rounds down positive number`` () =
        let result = eval <@ floor<float> 3.7 @> :?> float
        floatEqual 3.0 result 1e-10

    [<Fact>]
    let ``Q: floor rounds down negative number`` () =
        let result = eval <@ floor<float> -2.3 @> :?> float
        floatEqual -3.0 result 1e-10

    [<Fact>]
    let ``Q: floor keeps integer unchanged`` () =
        let result = eval <@ floor<float> 5.0 @> :?> float
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``Q: floor of 0.5`` () =
        let result = eval <@ floor<float> 0.5 @> :?> float
        floatEqual 0.0 result 1e-10

    [<Fact>]
    let ``Q: floor of -0.5`` () =
        let result = eval <@ floor<float> -0.5 @> :?> float
        floatEqual -1.0 result 1e-10

    [<Fact>]
    let ``Q: epsilon returns a very small positive value`` () =
        let result = eval <@ epsilon<float> () @> :?> float
        Assert.True(result > 0.0)
        Assert.True(result < 1e-300)

    // Edge case and mathematical property tests
    [<Fact>]
    let ``Q: exp(log(x)) = x`` () =
        let x = 5.0
        let result = eval <@ exp<float> (log<float> x) @> :?> float
        floatEqual x result 1e-10

    [<Fact>]
    let ``Q: log(exp(x)) = x`` () =
        let x = 2.0
        let result = eval <@ log<float> (exp<float> x) @> :?> float
        floatEqual x result 1e-10

    [<Fact>]
    let ``Q: sqrt(x^2) = abs(x)`` () =
        let x = -3.0
        let result = eval <@ sqrt<float> (pow<float> x 2.0) @> :?> float
        let expected = eval <@ abs<float> x @> :?> float
        floatEqual expected result 1e-10

    [<Fact>]
    let ``Q: sin^2 + cos^2 = 1`` () =
        let x = 0.7
        let sinX = eval <@ sin<float> x @> :?> float
        let cosX = eval <@ cos<float> x @> :?> float
        floatEqual 1.0 (sinX * sinX + cosX * cosX) 1e-10

    [<Fact>]
    let ``Q: zero<float> + one<float> = 1.0`` () =
        let z = eval <@ zero<float> @> :?> float
        let o = eval <@ one<float> @> :?> float
        Assert.Equal(1.0, z + o)

    [<Fact>]
    let ``Q: pi() is approximately 3.14159`` () =
        let piVal = eval <@ pi<float> () @> :?> float
        Assert.True(piVal > 3.14)
        Assert.True(piVal < 3.15)

    [<Fact>]
    let ``Q: tau() is approximately 6.28318`` () =
        let tauVal = eval <@ tau<float> () @> :?> float
        Assert.True(tauVal > 6.28)
        Assert.True(tauVal < 6.29)

    [<Fact>]
    let ``Q: e() is approximately 2.71828`` () =
        let eVal = eval <@ e<float> () @> :?> float
        Assert.True(eVal > 2.71)
        Assert.True(eVal < 2.72)

    [<Fact>]
    let ``Q: floor is monotonic`` () =
        let x1 = 2.3
        let x2 = 2.8
        let f1 = eval <@ floor<float> x1 @> :?> float
        let f2 = eval <@ floor<float> x2 @> :?> float
        Assert.True(f1 <= f2)
