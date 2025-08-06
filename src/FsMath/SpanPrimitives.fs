namespace FsMath

open System
open System.Runtime.InteropServices

// Maybe Namespace renaming: FsMath.INumberPrimitives (type Span)

// previously I used memeory marshal to cast spans to vectors, but it seems unsafe and not recommended.
// let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())

type SpanINumberPrimitives =

    // # map #########################################

    // With source, destination offsets and length
    static member inline mapIntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            srcOffset: int,
            dstOffset: int,
            length: int
        ) : unit =

        if length = 0 then
            ()
        elif Numerics.Vector.IsHardwareAccelerated && length >= Numerics.Vector<'T>.Count then
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = length / simdWidth
            let ceiling = simdWidth * simdCount

            for i = 0 to simdCount - 1 do
                let srcIndex = srcOffset + i * simdWidth
                let dstIndex = dstOffset + i * simdWidth
                let v = Numerics.Vector<'T>(src.Slice(srcIndex, simdWidth))
                let r = fv v
                r.CopyTo(dst.Slice(dstIndex, simdWidth))

            for i = ceiling to length - 1 do
                dst.[dstOffset + i] <- f src.[srcOffset + i]
        else
            for i = 0 to length - 1 do
                dst.[dstOffset + i] <- f src.[srcOffset + i]

    
    // With source offset only
    static member inline mapIntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            srcOffset: int,
            length: int
        ) : unit =

        SpanINumberPrimitives.mapIntoUnchecked<'T>
            (fv, f, src, dst, srcOffset, 0, length)


    // Without offsets
    static member inline mapIntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>
        ) : unit =

        SpanINumberPrimitives.mapIntoUnchecked<'T>
            (fv, f, src, dst, 0, 0, src.Length)



    // With source, destination offsets and length
    static member inline mapInto<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            srcOffset: int,
            dstOffset: int,
            length: int
        ) : unit =

        if srcOffset < 0 || dstOffset < 0 || length < 0 ||
           srcOffset + length > src.Length ||
           dstOffset + length > dst.Length then
            invalidArg "length" "Invalid offset or length for given spans."

        SpanINumberPrimitives.mapIntoUnchecked<'T>
            (fv, f, src, dst, srcOffset, dstOffset, length)
    
    // With source offsets only
    static member inline mapInto<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            srcOffset: int,
            length: int
        ) : unit =

        if srcOffset < 0 || length < 0 || 
            srcOffset + length > src.Length ||
            length > dst.Length then
             invalidArg "length" "Invalid offset or length for given spans."

        SpanINumberPrimitives.mapIntoUnchecked<'T>
            (fv, f, src, dst, 0, 0, src.Length)

    // Without offsets
    static member inline mapInto<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>
        ) : unit =

        if src.Length <> dst.Length then
            invalidArg "dst" "Destination span must be same length as source span"

        SpanINumberPrimitives.mapIntoUnchecked<'T>
            (fv, f, src, dst, 0, 0, src.Length)

    // Map function that returns a new Span
    static member inline map<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)                              
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T>,
                 f: 'T -> 'T,
                 span: ReadOnlySpan<'T>) : Span<'T> =
        
        let dst = Span<'T>(Array.zeroCreate span.Length)
        SpanINumberPrimitives.mapIntoUnchecked<'T>
            (fv, f, span, dst, 0, 0, span.Length)
        dst
    
    // # map2 ########################################

    // With source1, source2, destination offsets and length
    static member inline map2IntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src1: ReadOnlySpan<'T>,
            src2: ReadOnlySpan<'T>,
            dst: Span<'T>,
            src1Offset: int,
            src2Offset: int,
            dstOffset: int,
            length: int
        ) : unit =

        if length = 0 then
            ()
        elif Numerics.Vector.IsHardwareAccelerated && length >= Numerics.Vector<'T>.Count then
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = length / simdWidth
            let ceiling = simdWidth * simdCount

            for i = 0 to simdCount - 1 do
                let src1Index = src1Offset + i * simdWidth
                let src2Index = src2Offset + i * simdWidth
                let dstIndex  = dstOffset + i * simdWidth
                let v1 = Numerics.Vector<'T>(src1.Slice(src1Index, simdWidth))
                let v2 = Numerics.Vector<'T>(src2.Slice(src2Index, simdWidth))
                let r  = fv v1 v2
                r.CopyTo(dst.Slice(dstIndex, simdWidth))

            for i = ceiling to length - 1 do
                dst.[dstOffset + i] <- f src1.[src1Offset + i] src2.[src2Offset + i]
        else
            for i = 0 to length - 1 do
                dst.[dstOffset + i] <- f src1.[src1Offset + i] src2.[src2Offset + i]


    // With source offsets only
    static member inline map2IntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src1: ReadOnlySpan<'T>,
            src2: ReadOnlySpan<'T>,
            dst: Span<'T>,
            src1Offset: int,
            src2Offset: int,
            length: int
        ) : unit =

        SpanINumberPrimitives.map2IntoUnchecked<'T>
            (fv, f, src1, src2, dst, src1Offset, src2Offset, 0, length)


    // Without offsets
    static member inline map2IntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src1: ReadOnlySpan<'T>,
            src2: ReadOnlySpan<'T>,
            dst: Span<'T>
        ) : unit =

        SpanINumberPrimitives.map2IntoUnchecked<'T>
            (fv, f, src1, src2, dst, 0, 0, 0, src1.Length)


    // With source1, source2, destination offsets and length
    static member inline map2Into<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src1: ReadOnlySpan<'T>,
            src2: ReadOnlySpan<'T>,
            dst: Span<'T>,
            src1Offset: int,
            src2Offset: int,
            dstOffset: int,
            length: int
        ) : unit =

        if src1Offset < 0 || src2Offset < 0 || dstOffset < 0 || length < 0 ||
           src1Offset + length > src1.Length ||
           src2Offset + length > src2.Length ||
           dstOffset + length > dst.Length then
            invalidArg "length" "Invalid offset or length for given spans."

        SpanINumberPrimitives.map2IntoUnchecked<'T>
            (fv, f, src1, src2, dst, src1Offset, src2Offset, dstOffset, length)


    // With source offsets only
    static member inline map2Into<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src1: ReadOnlySpan<'T>,
            src2: ReadOnlySpan<'T>,
            dst: Span<'T>,
            src1Offset: int,
            src2Offset: int,
            length: int
        ) : unit =

        if src1Offset < 0 || src2Offset < 0 || length < 0 ||
           src1Offset + length > src1.Length ||
           src2Offset + length > src2.Length ||
           length > dst.Length then
            invalidArg "length" "Invalid offset or length for given spans."

        SpanINumberPrimitives.map2IntoUnchecked<'T>
            (fv, f, src1, src2, dst, src1Offset, src2Offset, 0, length)


    // Without offsets
    static member inline map2Into<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src1: ReadOnlySpan<'T>,
            src2: ReadOnlySpan<'T>,
            dst: Span<'T>
        ) : unit =

        if src1.Length <> src2.Length || src1.Length <> dst.Length then
            invalidArg "dst" "All spans must have the same length."

        SpanINumberPrimitives.map2IntoUnchecked<'T>
            (fv, f, src1, src2, dst, 0, 0, 0, src1.Length)


    // Map2 function that returns a new Span
    static member inline map2<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)                              
                and 'T : struct
                and 'T :> ValueType>
                (
                    fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
                    f: 'T -> 'T -> 'T,
                    src1: ReadOnlySpan<'T>,
                    src2: ReadOnlySpan<'T>
                ) : 'T[] =
        
        if src1.Length <> src2.Length then
            invalidArg "src2" "Source spans must have equal length"

        let result = Array.zeroCreate<'T>(src1.Length)
        let dst = Span<'T>(result)
        SpanINumberPrimitives.map2IntoUnchecked<'T>
            (fv, f, src1, src2, dst, 0, 0, 0, src1.Length)
        result

    // # mapScalar ########################################

    static member inline mapScalarIntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            scalar: 'T,
            srcOffset: int,
            dstOffset: int,
            length: int
        ) : unit =

        if length = 0 then
            ()
        elif Numerics.Vector.IsHardwareAccelerated && length >= Numerics.Vector<'T>.Count then
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = length / simdWidth
            let ceiling = simdWidth * simdCount
            let scalarVec = Numerics.Vector<'T>(scalar)

            for i = 0 to simdCount - 1 do
                let srcIndex = srcOffset + i * simdWidth
                let dstIndex = dstOffset + i * simdWidth
                let v = Numerics.Vector<'T>(src.Slice(srcIndex, simdWidth))
                let r = fv v scalarVec
                r.CopyTo(dst.Slice(dstIndex, simdWidth))

            for i = ceiling to length - 1 do
                dst.[dstOffset + i] <- f src.[srcOffset + i] scalar
        else
            for i = 0 to length - 1 do
                dst.[dstOffset + i] <- f src.[srcOffset + i] scalar


    static member inline mapScalarIntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            scalar: 'T,
            srcOffset: int,
            length: int
        ) : unit =
        SpanINumberPrimitives.mapScalarIntoUnchecked
            (fv, f, src, dst, scalar, srcOffset, 0, length)


    static member inline mapScalarIntoUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            scalar: 'T
        ) : unit =
        SpanINumberPrimitives.mapScalarIntoUnchecked
            (fv, f, src, dst, scalar, 0, 0, src.Length)


    static member inline mapScalarInto<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            scalar: 'T,
            srcOffset: int,
            dstOffset: int,
            length: int
        ) : unit =
        if srcOffset < 0 || dstOffset < 0 || length < 0 ||
           srcOffset + length > src.Length || dstOffset + length > dst.Length then
            invalidArg "length" "Invalid offset or length for given spans."
        SpanINumberPrimitives.mapScalarIntoUnchecked
            (fv, f, src, dst, scalar, srcOffset, dstOffset, length)


    static member inline mapScalarInto<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            dst: Span<'T>,
            scalar: 'T
        ) : unit =
        if src.Length <> dst.Length then
            invalidArg "dst" "Source and destination span must have the same length."
        SpanINumberPrimitives.mapScalarIntoUnchecked
            (fv, f, src, dst, scalar, 0, 0, src.Length)


    static member inline mapScalar<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            scalar: 'T
        ) : 'T[] =
        let result = Array.zeroCreate src.Length
        let dst = Span<'T>(result)
        SpanINumberPrimitives.mapScalarIntoUnchecked
            (fv, f, src, dst, scalar, 0, 0, src.Length)
        result


    // # fold ########################################

    static member inline foldUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            init: 'T,
            offset: int,
            length: int
        ) : 'T =

        if length = 0 then init
        elif Numerics.Vector.IsHardwareAccelerated && length >= Numerics.Vector<'T>.Count then
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = length / simdWidth
            let ceiling = simdWidth * simdCount

            // SIMD accumulation
            let mutable accVec = Numerics.Vector<'T>(init)

            for i = 0 to simdCount - 1 do
                let srcIndex = offset + i * simdWidth
                let v = Numerics.Vector<'T>(src.Slice(srcIndex, simdWidth))
                accVec <- fv accVec v

            // Reduce SIMD vector to scalar state
            let mutable acc = init
            for i = 0 to Numerics.Vector<'State>.Count - 1 do
                acc <- f acc accVec.[i]

            // Tail
            for i = ceiling to length - 1 do
                acc <- f acc src.[offset + i]

            acc
        else
            let mutable acc = init
            for i = 0 to length - 1 do
                acc <- f acc src.[offset + i]
            acc


    // Offset-only
    static member inline foldUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            init: 'T,
            offset: int
        ) : 'T =
        SpanINumberPrimitives.foldUnchecked(fv, f, src, init, offset, src.Length - offset)


    // No offset
    static member inline foldUnchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            init: 'T
        ) : 'T =
        SpanINumberPrimitives.foldUnchecked(fv, f, src, init, 0, src.Length)


    // Checked version
    static member inline fold<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            init: 'T,
            offset: int,
            length: int
        ) : 'T =

        if offset < 0 || length < 0 || offset + length > src.Length then
            invalidArg "length" "Invalid offset or length"
        SpanINumberPrimitives.foldUnchecked(fv, f, src, init, offset, length)


    // Checked: offset only
    static member inline fold<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            init: 'T,
            offset: int
        ) : 'T =
        if offset < 0 || offset > src.Length then
            invalidArg "offset" "Offset out of range"
        SpanINumberPrimitives.foldUnchecked(fv, f, src, init, offset, src.Length - offset)
        

    // Checked: no offset
    static member inline fold<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new : unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T,
            src: ReadOnlySpan<'T>,
            init: 'T
        ) : 'T =
        SpanINumberPrimitives.foldUnchecked(fv, f, src, init, 0, src.Length)


    // # fold2 ########################################

    static member inline fold2Unchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new: unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T -> 'T,
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            init: 'T,
            xOffset: int,
            yOffset: int,
            length: int
        ) : 'T =

        if length = 0 then init
        elif Numerics.Vector.IsHardwareAccelerated && length >= Numerics.Vector<'T>.Count then
            let simdWidth = Numerics.Vector<'T>.Count
            let simdCount = length / simdWidth
            let ceiling = simdWidth * simdCount

            let mutable accVec = Numerics.Vector<'T>(init)

            for i = 0 to simdCount - 1 do
                let xi = xOffset + i * simdWidth
                let yi = yOffset + i * simdWidth
                let vx = Numerics.Vector<'T>(x.Slice(xi, simdWidth))
                let vy = Numerics.Vector<'T>(y.Slice(yi, simdWidth))
                accVec <- fv accVec vx vy

            let mutable acc = init
            for i = 0 to Numerics.Vector<'T>.Count - 1 do
                acc <- acc + accVec.[i]

            for i = ceiling to length - 1 do
                acc <- f acc x.[xOffset + i] y.[yOffset + i]

            acc
        else
            let mutable acc = init
            for i = 0 to length - 1 do
                acc <- f acc x.[xOffset + i] y.[yOffset + i]
            acc


    // Offset only
    static member inline fold2Unchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new: unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T -> 'T,
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            init: 'T,
            xOffset: int,
            yOffset: int
        ) : 'T =
        SpanINumberPrimitives.fold2Unchecked(fv, f, x, y, init, xOffset, yOffset, x.Length - xOffset)


    // No offset
    static member inline fold2Unchecked<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new: unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T -> 'T,
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            init: 'T
        ) : 'T =
        SpanINumberPrimitives.fold2Unchecked(fv, f, x, y, init, 0, 0, x.Length)


    // Checked: full parameters
    static member inline fold2<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new: unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T -> 'T,
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            init: 'T,
            xOffset: int,
            yOffset: int,
            length: int
        ) : 'T =

        if xOffset < 0 || yOffset < 0 || length < 0 ||
           xOffset + length > x.Length || yOffset + length > y.Length then
            invalidArg "length" "Invalid offset or length for fold2 spans."

        SpanINumberPrimitives.fold2Unchecked(fv, f, x, y, init, xOffset, yOffset, length)


    // Checked: offset only
    static member inline fold2<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new: unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T -> 'T,
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            init: 'T,
            xOffset: int,
            yOffset: int
        ) : 'T =
        SpanINumberPrimitives.fold2(fv, f, x, y, init, xOffset, yOffset, x.Length - xOffset)


    // Checked: no offset
    static member inline fold2<'T
        when 'T :> Numerics.INumber<'T>
         and 'T : (new: unit -> 'T)
         and 'T : struct
         and 'T :> ValueType>
        (
            fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
            f: 'T -> 'T -> 'T -> 'T,
            x: ReadOnlySpan<'T>,
            y: ReadOnlySpan<'T>,
            init: 'T
        ) : 'T =
        if x.Length <> y.Length then
            invalidArg "y" "Both spans must have equal length"
        SpanINumberPrimitives.fold2Unchecked(fv, f, x, y, init, 0, 0, x.Length)



