namespace FsMath

open System
open System.Runtime.InteropServices

type SpanMath =
    
    // # dot ########################################

    static member inline dotUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            xOffset: int,
            yOffset: int,
            length: int
        ) : 'T =

        if length = 0 then
            LanguagePrimitives.GenericZero
        elif Numerics.Vector.IsHardwareAccelerated && length >= Numerics.Vector<'T>.Count then
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = length / simdWidth
            let ceiling   = simdWidth * simdCount

            let mutable accVec = Numerics.Vector<'T>.Zero

            for i = 0 to simdCount - 1 do
                let xi = xOffset + i * simdWidth
                let yi = yOffset + i * simdWidth
                let vx = Numerics.Vector<'T>(x.Slice(xi, simdWidth))
                let vy = Numerics.Vector<'T>(y.Slice(yi, simdWidth))
                accVec <- accVec + (vx * vy)

            let mutable acc = LanguagePrimitives.GenericZero<'T>
            for i = 0 to simdWidth - 1 do
                acc <- acc + accVec.[i]

            for i = ceiling to length - 1 do
                acc <- acc + x.[xOffset + i] * y.[yOffset + i]

            acc
        else
            let mutable acc = LanguagePrimitives.GenericZero<'T>
            for i = 0 to length - 1 do
                acc <- acc + x.[xOffset + i] * y.[yOffset + i]
            acc


    static member inline dotUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            xOffset: int,
            length: int
        ) : 'T =
        SpanMath.dotUnchecked(x, y, xOffset, xOffset, length)


    static member inline dotUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>
        ) : 'T =
        SpanMath.dotUnchecked(x, y, 0, 0, x.Length)


    // Checked version with all offsets
    static member inline dot<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            xOffset: int,
            yOffset: int,
            length: int
        ) : 'T =

        if xOffset < 0 || yOffset < 0 || length < 0 ||
           xOffset + length > x.Length ||
           yOffset + length > y.Length then
            invalidArg "length" "Invalid offset or length for dot product."

        SpanMath.dotUnchecked(x, y, xOffset, yOffset, length)


    // Checked version with single offset
    static member inline dot<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            offset: int
        ) : 'T =
        SpanMath.dot(x, y, offset, offset, x.Length - offset)


    // Checked version with no offsets
    static member inline dot<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>
        ) : 'T =
        if x.Length <> y.Length then
            invalidArg "y" "Input spans must be equal length."

        SpanMath.dotUnchecked(x, y, 0, 0, x.Length)


    // # add ########################################
    // Adds two spans element-wise, returning a new array.
    static member inline add<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>
        ) : 'T[] =
        
        if x.Length <> y.Length then
            invalidArg "" "Cannot add two vectors of different dimensions."

        SpanINumberPrimitives.map2( (+) , (+) , x , y )
        
    // # addScalar ########################################
    // Adds a scalar to each element of the span
    static member inline addScalar<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            scalar : 'T
        ) : 'T[] =

        SpanINumberPrimitives.mapScalar( (+) , (+) , x , scalar )

    // # Subtract ########################################
    // Subtracts two vectors element-wise
    static member inline subtract<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>
        ) : 'T[] =
        
        if x.Length <> y.Length then
            invalidArg "" "Cannot subtract two vectors of different dimensions."

        SpanINumberPrimitives.map2( (-) , (-) , x , y )
        
    // # SubtractScalar ########################################
    // Subtracts a scalar from each element in the span
    static member inline subtractScalar<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            scalar : 'T
        ) : 'T[] =

        SpanINumberPrimitives.mapScalar( (-) , (-) , x , scalar )

    // # Multiply ########################################
    // Multiplies two spans element-wise, returning a new array.
    static member inline multiply<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>
        ) : 'T[] =
        
        if x.Length <> y.Length then
            invalidArg "" "Cannot multiply two vectors of different dimensions."
        SpanINumberPrimitives.map2( (*) , (*) , x , y )

    // # MultiplyScalar ########################################
    // Multiplies each element of the span by a scalar
    static member inline multiplyScalar<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            scalar : 'T
        ) : 'T[] =
        SpanINumberPrimitives.mapScalar( (*) , (*) , x , scalar )

    // # Divide ########################################
    // Divides two spans element-wise, returning a new array.
    static member inline divide<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>
        ) : 'T[] =
        
        if x.Length <> y.Length then
            invalidArg "" "Cannot divide two vectors of different dimensions."
        SpanINumberPrimitives.map2( (/) , (/) , x , y )

    // # DivideScalar ########################################
    // Divides each element of the span by a scalar
    static member inline divideScalar<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new : unit -> 'T)
         and 'T :> ValueType>
        (
            x: ReadOnlySpan<'T>,
            scalar : 'T
        ) : 'T[] =
        SpanINumberPrimitives.mapScalar( (/) , (/) , x , scalar )

    /// Computes the sum of all elements in the vector.
    static member inline sum<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (v:ReadOnlySpan<'T>) : 'T =
        let zero =  LanguagePrimitives.GenericZero<'T>
        SpanINumberPrimitives.fold ( (+) , (+) , v , zero )


    /// Computes the product of all elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The product of all elements in the vector.</returns>
    static member inline product<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (v:ReadOnlySpan<'T>) : 'T =
        let one =  LanguagePrimitives.GenericOne<'T>
        SpanINumberPrimitives.fold ( (*) , (*) , v , one )

        

    /// Computes the mean of the elements in the vector.
    static member inline mean<'T when 'T :> Numerics.INumber<'T>
                and 'T : (static member DivideByInt : 'T * int -> 'T)
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (v: ReadOnlySpan<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute mean of empty vector."
        let zero =  LanguagePrimitives.GenericZero<'T>
        let sum  = SpanINumberPrimitives.fold ( (+) , (+) , v , zero )
        LanguagePrimitives.DivideByInt<'T> sum v.Length


    /// Computes the Euclidean norm (L2 norm) of a span
    static member inline norm<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
                (v:ReadOnlySpan<'T>) : 'T =
        
        let sumSquares  = SpanMath.dot( v, v)
        'T.Sqrt sumSquares

    /// Computes the manimum value in the span.
    static member inline min<'T when 'T :> Numerics.INumber<'T>
                 and 'T : (new: unit -> 'T)
                 and 'T : struct
                 and 'T : comparison
                 and 'T :> ValueType> 
                 (v:ReadOnlySpan<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute min of empty vector."
        SpanINumberPrimitives.fold ((fun a b -> Numerics.Vector.Min(a, b)), min, v, v.[0])

    /// Computes the maximum value in the span.
    static member inline max<'T when 'T :> Numerics.INumber<'T>
                 and 'T : (new: unit -> 'T)
                 and 'T : struct
                 and 'T : comparison
                 and 'T :> ValueType> 
                 (v:ReadOnlySpan<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute min of empty vector."
        SpanINumberPrimitives.fold ((fun a b -> Numerics.Vector.Max(a, b)), max, v, v.[0])


// outer product #######
    
    /// Computes the outer product of two spans.
    /// Result[i,j] = u[i] * v[j] for all i,j
    static member inline outerProduct<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : struct
         and 'T : (new: unit -> 'T)
         and 'T : comparison
         and 'T :> ValueType>
        (
            u: ReadOnlySpan<'T>,  // column vector
            v: ReadOnlySpan<'T>   // row vector
        ) : int*int*'T[] =

        let rows = u.Length
        let cols = v.Length
        let data = Array.zeroCreate<'T> (rows * cols)

        if Numerics.Vector.IsHardwareAccelerated && cols >= Numerics.Vector<'T>.Count then
            // SIMD path: broadcast each u[i] and multiply with v vector
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = cols / simdWidth
            let ceiling = simdCount * simdWidth

            // Cast v to SIMD vectors once
            let vVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v)

            for i = 0 to rows - 1 do
                let ui = u[i]
                let uBroadcast = Numerics.Vector<'T>(ui)
                let rowStart = i * cols

                // SIMD multiplication for aligned portion
                for k = 0 to simdCount - 1 do
                    let result = uBroadcast * vVec[k]
                    result.CopyTo(MemoryMarshal.CreateSpan(&data.[rowStart + k * simdWidth], simdWidth))

                // Scalar fallback for remainder
                for j = ceiling to cols - 1 do
                    data.[rowStart + j] <- ui * v.[j]
        else
            // Scalar fallback for small vectors or no SIMD
            for i = 0 to rows - 1 do
                let ui = u[i]
                let rowStart = i * cols
                for j = 0 to cols - 1 do
                    data.[rowStart + j] <- ui * v.[j]

        (rows, cols, data)
