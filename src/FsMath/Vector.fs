namespace FsMath

open FsMath
open System
open System.Runtime.InteropServices

/// Vector as a Array type alias
type Vector<'T when 'T :> Numerics.INumber<'T>> = 'T []

type Vector() =


    static member inline add<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        
        if v1.Length <> v2.Length then
            invalidArg "" "Cannot add two vectors of different dimensions."
        
        SIMDUtils.map2Unchecked (+) (+) v1 v2
 
    static member inline subtract<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =

        if v1.Length <> v2.Length then
            invalidArg "v2" "Arrays must have the same length."
        
        SIMDUtils.map2Unchecked (-) (-) v1 v2

    static member inline multiply<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =

        if v1.Length <> v2.Length then
            invalidArg "v2" "Arrays must have the same length."
        
        SIMDUtils.map2Unchecked ( * ) ( * ) v1 v2


    static member inline divide<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        if v1.Length <> v2.Length then
            invalidArg "v2" "Arrays must have the same length."
        SIMDUtils.map2Unchecked (/) (/) v1 v2


    static member inline addScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1:Vector<'T>) (scalar:'T) : Vector<'T> =
        
        SIMDUtils.mapScalar (+) (+) v1 scalar

    static member inline subtractScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1:Vector<'T>) (scalar:'T) : Vector<'T> =
        
        SIMDUtils.mapScalar (-) (-) v1 scalar

    static member inline multiplyScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1:Vector<'T>) (scalar:'T) : Vector<'T> =
        
        SIMDUtils.mapScalar ( * ) ( * ) v1 scalar

    static member inline divideScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1:Vector<'T>) (scalar:'T) : Vector<'T> =
        
        SIMDUtils.mapScalar (/) (/) v1 scalar


    static member inline sum<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (v:Vector<'T>) : 'T =
        let zero =  LanguagePrimitives.GenericZero<'T>
        SIMDUtils.fold (+) (+) zero v

    static member inline product<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (v:Vector<'T>) : 'T =
        let one =  LanguagePrimitives.GenericOne<'T>
        SIMDUtils.fold (*) (*) one v

    static member inline mean<'T when 'T :> Numerics.INumber<'T>
                and 'T : (static member DivideByInt : 'T * int -> 'T)
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (v: Vector<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute mean of empty vector."
        let sum = Vector.sum v
        LanguagePrimitives.DivideByInt<'T> sum v.Length


    static member inline dotProduct<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : 'T =

        if v1.Length <> v2.Length then
            invalidArg "v2" "Arrays must have the same length."
        
        let length = v1.Length
        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let mutable result = Numerics.Vector<'T>.Zero
        let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
        let v2Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v2.AsSpan())
        let mutable scalarResult = LanguagePrimitives.GenericZero<'T>

        for i = 0 to slotCount - 1 do
            result <- Numerics.Vector.Add(result, Numerics.Vector.Multiply(v1Span.[i], v2Span.[i])) 
            scalarResult <- Numerics.Vector.Sum result
        
        for i = ceiling to v1.Length - 1 do
            scalarResult <- scalarResult + (v1.[i] * v2.[i])

        scalarResult

    static member inline norm<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
                (v:Vector<'T>) : 'T =
        
        let sumSquares  = Vector.dotProduct v v
        GenericMath.sqrt sumSquares 

    // static member inline min<'T when 'T :> Numerics.INumber<'T>
    //             and 'T : (new: unit -> 'T)
    //             and 'T : struct
    //             and 'T :> ValueType> 
    //             (v:Vector<'T>) : 'T =
    //     if v.Length = 0 then invalidArg "v" "Cannot compute min of empty vector."
    //     SIMDUtils.fold (Vector.Min) min v.[0] v

    // static member inline powScalar (v: float32[]) (scalar:float32) : float32[] =
    //     SIMDMath.powF32 v scalar
        
    // static member inline powScalar (v: float[]) (scalar:float) : float[] =
    //     SIMDMath.powF64 v scalar



