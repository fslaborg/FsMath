namespace FsMath.SpecialFunctions


open System
open FsMath
open FsMath.GenericMath

/// Approximations for the gamma function and related functions.
///
/// The gamma function (represented by Γ, the capital letter gamma from the Greek alphabet) is one commonly used extension 
/// of the factorial function to complex numbers:
///
/// Γ(x) = (x-1)!
///
///The gamma function is defined for all complex numbers except the non-positive integers.
type Gamma =
    
    /// <summary>Maximum gamma</summary>
    static member maximum = 171.624376956302725

    ///<summary>
    /// Computes an approximation of the real value of the gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks> 
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</remarks> 
    /// <param name="z">The function input for approximating Γ(z)</param>    
    static member inline _gamma<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> > (z: 'T) : 'T =
        let coeffs : Vector<'T> =
            [|
                T 76.18009172947146
                T -86.50532032941677
                T 24.01409824083091
                T -1.231739572450155
                T 0.001208650973866179
                T -0.000005395239384953
            |]

        let x = z - one
        let g = T 5.0
        let half = T 0.5
        let xg = x + g + half

        let sum =
            coeffs
            |> Vector.foldi (fun i acc c -> 
                acc + c / (x + T (float (i + 1)))) (T 1.000000000190015)

        pow xg (x + half)
        * exp (-xg)
        * sqrt ('T.Pi * T 2.0)
        * sum

    ///<summary>
    /// Computes an approximation of the real value of the log gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al)
    ///</summary>
    ///<remarks>
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</remarks>
    /// <param name="z">The function input for approximating ln(Γ(z))</param>
    static member inline _gammaLn<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> > (z: 'T) : 'T =
        let coeffs =
            [|
                T 76.18009172947146
                T -86.50532032941677
                T 24.01409824083091
                T -1.231739572450155
                T 0.001208650973866179
                T -0.000005395239384953
            |]

        let pi = 'T.Pi
        let x = z - one
        let g = T 5.0
        let half = T 0.5
        let xg = x + g + half

        // let sum =
        //     coeffs
        //     |> Vector.foldi (fun i acc c -> 
        //         acc + c / (x + T (float (i + 1)))) (T 1.000000000190015)
        
        // //(x + half) * log xg - xg + log (sqrt ('T.Pi * T<'T> 2.0) * sum)
        // (x + half) * log xg - xg + T 0.5 * log (T (2.0 * System.Math.PI)) + log sum
        let sum =
            coeffs
            |> Vector.foldi (fun i acc c -> acc + c / (z + T (float i))) (T 1.000000000190015)

        (x + half) * log xg
        - xg
        + half * log (T 2.0 * pi)
        + log sum


    ///<summary>
    /// Computes an approximation of the real value of the gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks>
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    /// This might be slower than the unchecked version `_gamma` but does not require input sanitation to get expected results for these cases.
    ///</remarks>
    /// <param name="z">The function input for approximating Γ(z)</param>
    static member inline gamma<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : equality > (z: 'T) : 'T =

        match z with
        | z when 'T.IsPositiveInfinity z -> z
        | z_ when 'T.IsNegativeInfinity z -> T nan
        | z when 'T.IsNaN z              -> z
        | z -> Gamma._gamma z



    ///<summary>
    /// Computes an approximation of the real value of the log gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al)
    ///</summary>
    ///<remarks>
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    /// This might be slower than the unchecked version `_gamma` but does not require input sanitation to get expected results for these cases.
    ///</remarks>
    /// <param name="z">The function input for approximating ln(Γ(z))</param>
    static member inline gammaLn<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : equality > (z: 'T) : 'T =
        
        match z with
        | z when 'T.IsPositiveInfinity z -> z
        | z when 'T.IsNegativeInfinity z -> T<'T> System.Double.NaN
        | z when 'T.IsNaN z              -> z
        | z -> Gamma._gammaLn z



    /// <summary>
    /// Series representation of the lower incomplete gamma function P(a,x)
    /// </summary>
    /// <param name="a">Shape parameter</param>
    /// <param name="x">Integration upper limit</param>
    /// <returns>Regularized lower incomplete gamma value</returns>
    static member inline gser<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) (x: 'T) : 'T =

        // Relative precision threshold used in convergence tests
        let EPS = T<'T> 3.0e-8

        let gln = Gamma._gammaLn a
        let rec loop sum del ap =
            let ap' = ap + 'T.One
            let del' = del * x / ap'
            if abs del < abs sum * EPS then
                sum * exp (-x + a * log x - gln)
            else
                loop (sum + del') del' ap'

        loop (T<'T> 1.0 / a) (T<'T> 1.0 / a) a


    /// <summary>
    /// Continued fraction representation of the upper incomplete gamma function Q(a,x)
    /// </summary>
    /// <param name="a">Shape parameter</param>
    /// <param name="x">Integration upper limit</param>
    /// <returns>Regularized upper incomplete gamma value</returns>
    static member inline gcf<'T when 'T :>  Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) (x: 'T) : 'T =

        // Smallest positive number to avoid division underflow in continued fractions
        let FPMIN = T<'T> 1.0e-30
        // Relative precision threshold used in convergence tests
        let EPS = T<'T> 3.0e-8

        let gln = Gamma._gammaLn a
        let one = 'T.One
        let b0 = x + one - a
        let c0 = one / FPMIN
        let d0 = one / b0
        let h0 = d0

        let rec loop i b c d h =
            let an = -i * (i - a)
            let b = b + T<'T> 2.0
            let d =
                let tmp = an * d + b
                if abs tmp < FPMIN then FPMIN else tmp
            let c =
                let tmp = b + an / c
                if abs tmp < FPMIN then FPMIN else tmp
            let d = one / d
            let del = d * c
            let h = h * del

            if abs (del - one) <= EPS then
                exp (-x + a * log x - gln) * h
            else
                loop (i + one) b c d h

        loop one b0 c0 d0 h0


    /// <summary>
    /// Gauss-Legendre quadrature approximation for the regularized gamma function
    /// </summary>
    /// <param name="a">Shape parameter</param>
    /// <param name="x">Evaluation point</param>
    /// <param name="psig">Return P(a,x) if true, Q(a,x) if false</param>
    /// <returns>Approximated value for P(a,x) or Q(a,x)</returns>
    static member inline gammpapprox<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) (x: 'T) (psig:bool) : 'T =

        // Abscissas for Gauss-Legendre quadrature
        let gaussLegQuadY = [|
            0.0021695375159141994; 0.011413521097787704; 0.027972308950302116;
            0.051727015600492421; 0.082502225484340941; 0.12007019910960293;
            0.16415283300752470; 0.21442376986779355; 0.27051082840644336;
            0.33199876341447887; 0.39843234186401943; 0.46931971407375483;
            0.54413605556657973; 0.62232745288031077; 0.70331500465597174;
            0.78649910768313447; 0.87126389619061517; 0.95698180152629142 |]

        // Weights for Gauss-Legendre quadrature
        let gaussLegQuadWeights = [|
            0.0055657196642445571; 0.012915947284065419; 0.020181515297735382;
            0.027298621498568734; 0.034213810770299537; 0.040875750923643261;
            0.047235083490265582; 0.053244713977759692; 0.058860144245324798;
            0.064039797355015485; 0.068745323835736408; 0.072941885005653087;
            0.076598410645870640; 0.079687828912071670; 0.082187266704339706;
            0.084078218979661945; 0.085346685739338721; 0.085983275670394821 |]

        let ngau = gaussLegQuadWeights.Length
        let a1 = a - 'T.One
        let lna1 = log a1
        let sqrta1 = sqrt a1
        let gln = Gamma._gammaLn a

        let xu =
            if x > a1 then
                max (a1 + T<'T> 11.5 * sqrta1) (x + T<'T> 6.0 * sqrta1)
            else
                max 'T.Zero (min (a1 - T<'T> 7.5 * sqrta1) (x - T<'T> 5.0 * sqrta1))

        let rec gaussLegendre j sum =
            if j < ngau then
                let t = x + (xu - x) * T<'T> gaussLegQuadY.[j]
                let w = T<'T> gaussLegQuadWeights.[j]
                let f = exp (-(t - a1) + a1 * (log t - lna1))
                gaussLegendre (j + 1) (sum + w * f)
            else
                let ans = sum * (xu - x) * exp (a1 * (lna1 - 'T.One) - gln)
                if psig then
                    if ans > 'T.Zero then 'T.One - ans else -ans
                else
                    if ans >= 'T.Zero then ans else 'T.One + ans

        gaussLegendre 0 'T.Zero


    /// <summary>
    /// Regularized lower incomplete gamma function P(a,x)
    /// </summary>
    /// <param name="a">Shape parameter</param>
    /// <param name="x">Integration upper limit</param>
    /// <returns>P(a,x), the lower incomplete gamma function divided by Γ(a)</returns>
    static member inline lowerIncompleteRegularized<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) (x: 'T) : 'T =

        let ASWITCH = T<'T> 100.0
        match x with
        | _ when x = 'T.Zero -> 'T.Zero
        | _ when x < 'T.Zero || a <= 'T.Zero -> T nan
        | _ when 'T.IsPositiveInfinity x -> 'T.One
        | _ ->
            if a >= ASWITCH then
                Gamma.gammpapprox a x true
            elif x < a + 'T.One then
                Gamma.gser a x
            else
                'T.One - Gamma.gcf a x



    /// <summary>
    /// Regularized upper incomplete gamma function Q(a,x)
    /// </summary>
    /// <param name="a">Shape parameter</param>
    /// <param name="x">Integration upper limit</param>
    /// <returns>Q(a,x) = 1 - P(a,x)</returns>
    static member inline upperIncompleteRegularized<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) ( x: 'T) : 'T =

        let ASWITCH = T<'T> 100.0
        match x with
        | _ when x = 'T.Zero -> 'T.One
        | _ when x < 'T.Zero || a <= 'T.Zero -> T nan
        | _ when 'T.IsPositiveInfinity x -> 'T.Zero
        | _ ->
            if a >= ASWITCH then
                'T.One - Gamma.gammpapprox a x true
            elif x < a + 'T.One then
                'T.One - Gamma.gser a x
            else
                Gamma.gcf a x


    /// <summary>
    /// Unregularized lower incomplete gamma function γ(a, x)
    /// </summary>
    /// <param name="a">The shape parameter a (must be positive).</param>
    /// <param name="x">The integration limit x (must be ≥ 0).</param>
    /// <returns>The value of the lower incomplete gamma function γ(a,x)</returns>
    static member inline lowerIncomplete<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) (x: 'T) : 'T =
        
        (Gamma.lowerIncompleteRegularized a x) * (exp (Gamma._gammaLn a))

    /// <summary>
    /// Computes the unregularized upper incomplete gamma function Γ̄(a, x) = Γ(a) · Q(a, x),
    /// where Q(a, x) is the regularized upper incomplete gamma function.
    /// </summary>
    /// <param name="a">The shape parameter a (must be positive).</param>
    /// <param name="x">The lower limit of integration x (must be ≥ 0).</param>
    /// <returns>The unregularized upper incomplete gamma function Γ̄(a, x).</returns>
    static member inline upperIncomplete<'T when 'T :> Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T : comparison > (a: 'T) (x: 'T) : 'T =

        (Gamma.upperIncompleteRegularized a x) * (exp (Gamma._gammaLn a))


    /// <summary>
    /// Optimized digamma function for x > 1. Skips reflection and tan-based correction.
    /// Use when x is guaranteed to be greater than 1.
    /// </summary>
    /// <param name="x">The input value (must be > 1).</param>
    /// <returns>The value of the digamma function ψ(x).</returns>
    static member inline digammaPositive<'T when 'T :>  Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T : comparison > (x: 'T) : 'T =

        let pi = 'T.Pi 
        let half = T<'T> 0.5
        let one = 'T.One
        let zero = 'T.Zero
        let ln = log<'T>

        let polyCoeffs =
            [|
                T 8.33333333333333333333E-2
                T -2.10927960927960927961E-2
                T 7.57575757575757575758E-3
                T -4.16666666666666666667E-3
                T 3.96825396825396825397E-3
                T -8.33333333333333333333E-3
                T 8.33333333333333333333E-2
            |]

        let ten = T<'T> 10.0
        let limit = T<'T> 1.0e17

        let mutable s = x
        let mutable w = zero

        while s < ten do
            w <- w + one / s
            s <- s + one

        if s < limit then
            let z = one / (s * s)
            let polv = polyCoeffs |> Array.fold (fun acc c -> acc * z + c) zero
            ln s - half / s - z * polv - w
        else
            ln s - half / s - w

    /// <summary>
    /// Full digamma function ψ(x),
    /// </summary>
    static member inline digamma<'T when 'T :>  Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and Numerics.ITrigonometricFunctions<'T>
        and 'T : comparison > (x: 'T) : 'T =

        let one = 'T.One
        let zero = 'T.Zero
        let half = T<'T> 0.5
        let ten = T<'T> 10.0
        let limit = T<'T> 1e17
        let pi = 'T.Pi

        let polyCoeffs =
            [|
                T 8.33333333333333333333E-2
                T -2.10927960927960927961E-2
                T 7.57575757575757575758E-3
                T -4.16666666666666666667E-3
                T 3.96825396825396825397E-3
                T -8.33333333333333333333E-3
                T 8.33333333333333333333E-2
            |]

        let input =
            if x < zero then
                one - x // reflect
            else
                x

        // Accumulate series if input is small
        let mutable s = input
        let mutable w = zero
        while s < ten do
            w <- w + one / s
            s <- s + one

        // Asymptotic expansion
        let result =
            if s < limit then
                let z = one / (s * s)
                let polv = polyCoeffs |> Array.fold (fun acc c -> acc * z + c) zero
                log<'T> s - half / s - z * polv - w
            else
                log<'T> s - half / s - w

        if x = zero then
            T System.Double.NegativeInfinity
        elif x < zero then
            result + pi / 'T.Tan (-pi * x)
        else
            result



    // /// <summary>
    // /// Full digamma function ψ(x), using fast path for x > 1 and reflection for x ≤ 0.
    // /// </summary>
    // static member inline digamma<'T when 'T :>  Numerics.INumber<'T>
    //     and Numerics.IFloatingPoint<'T>
    //     and Numerics.IExponentialFunctions<'T>
    //     and Numerics.ILogarithmicFunctions<'T>
    //     and Numerics.IRootFunctions<'T>
    //     and Numerics.IPowerFunctions<'T>
    //     and Numerics.ITrigonometricFunctions<'T> 
    //     and 'T : comparison > (x: 'T) : 'T =

    //     let inline recurse (x: 'T) : 'T =

    //         let one = 'T.One
    //         let zero = 'T.Zero
    //         let pi = 'T.Pi

    //         if x = zero then
    //             T System.Double.NegativeInfinity
    //         elif x < zero then
    //             let tan = tan<'T>
    //             recurse (one - x) + pi / tan (-pi * x)
    //         else
    //             Gamma.digammaPositive x

    //     recurse x


// regularizedGammaP and Q aliases?


