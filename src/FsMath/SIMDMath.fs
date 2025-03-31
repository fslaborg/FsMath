namespace FsMath.Acceleration

open FsMath
open System
open System.Runtime.InteropServices

module SIMDMath =


    // -------- float32 --------
    let powF32 (v: float32[]) (scalar: float32) : float32[] =
        let length = v.Length
        let slotSize = Numerics.Vector<float32>.Count
        let results = Array.zeroCreate<float32> length
        let rsSpan = MemoryMarshal.Cast<float32, Numerics.Vector<float32>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<float32, Numerics.Vector<float32>>(v.AsSpan())
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize
        for i = 0 to slotCount - 1 do
            let chunk = vSpan.[i]
            let temp = Array.init slotSize (fun j -> MathF.Pow(chunk.[j], scalar))
            rsSpan.[i] <- Numerics.Vector<float32>(temp)
        for i = ceiling to length - 1 do
            results.[i] <- MathF.Pow(v.[i], scalar)
        results

    let sqrtF32 (v: float32[]) : float32[] =
        let length = v.Length
        let slotSize = Numerics.Vector<float32>.Count
        let results = Array.zeroCreate<float32> length
        let rsSpan = MemoryMarshal.Cast<float32, Numerics.Vector<float32>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<float32, Numerics.Vector<float32>>(v.AsSpan())
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize
        for i = 0 to slotCount - 1 do
            let chunk = vSpan.[i]
            let temp = Array.init slotSize (fun j -> MathF.Sqrt(chunk.[j]))
            rsSpan.[i] <- Numerics.Vector<float32>(temp)
        for i = ceiling to length - 1 do
            results.[i] <- MathF.Sqrt(v.[i])
        results

    let expF32 (v: float32[]) : float32[] =
        let length = v.Length
        let slotSize = Numerics.Vector<float32>.Count
        let results = Array.zeroCreate<float32> length
        let rsSpan = MemoryMarshal.Cast<float32, Numerics.Vector<float32>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<float32, Numerics.Vector<float32>>(v.AsSpan())
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize
        for i = 0 to slotCount - 1 do
            let chunk = vSpan.[i]
            let temp = Array.init slotSize (fun j -> MathF.Exp(chunk.[j]))
            rsSpan.[i] <- Numerics.Vector<float32>(temp)
        for i = ceiling to length - 1 do
            results.[i] <- MathF.Exp(v.[i])
        results

    // -------- float --------
    let powF64 (v: float[]) (scalar: float) : float[] =
        let length = v.Length
        let slotSize = Numerics.Vector<float>.Count
        let results = Array.zeroCreate<float> length
        let rsSpan = MemoryMarshal.Cast<float, Numerics.Vector<float>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<float, Numerics.Vector<float>>(v.AsSpan())
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize
        for i = 0 to slotCount - 1 do
            let chunk = vSpan.[i]
            let temp = Array.init slotSize (fun j -> Math.Pow(chunk.[j], scalar))
            rsSpan.[i] <- Numerics.Vector<float>(temp)
        for i = ceiling to length - 1 do
            results.[i] <- Math.Pow(v.[i], scalar)
        results

    let sqrtF64 (v: float[]) : float[] =
        let length = v.Length
        let slotSize = Numerics.Vector<float>.Count
        let results = Array.zeroCreate<float> length
        let rsSpan = MemoryMarshal.Cast<float, Numerics.Vector<float>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<float, Numerics.Vector<float>>(v.AsSpan())
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize
        for i = 0 to slotCount - 1 do
            let chunk = vSpan.[i]
            let temp = Array.init slotSize (fun j -> Math.Sqrt(chunk.[j]))
            rsSpan.[i] <- Numerics.Vector<float>(temp)
        for i = ceiling to length - 1 do
            results.[i] <- Math.Sqrt(v.[i])
        results

    let expF64 (v: float[]) : float[] =
        let length = v.Length
        let slotSize = Numerics.Vector<float>.Count
        let results = Array.zeroCreate<float> length
        let rsSpan = MemoryMarshal.Cast<float, Numerics.Vector<float>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<float, Numerics.Vector<float>>(v.AsSpan())
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize
        for i = 0 to slotCount - 1 do
            let chunk = vSpan.[i]
            let temp = Array.init slotSize (fun j -> Math.Exp(chunk.[j]))
            rsSpan.[i] <-Numerics.Vector<float>(temp)
        for i = ceiling to length - 1 do
            results.[i] <- Math.Exp(v.[i])
        
        
        results


    // open System.Numerics
    // // -------- generic fallback --------
    // let inline powGeneric<'T when 'T :> Numerics.INumber<'T>
    //                              and 'T : (new: unit -> 'T)
    //                              and 'T : struct
    //                              and 'T :> ValueType> (v: 'T[]) (scalar: 'T) : 'T[] =
    //     let inline pow x y = (^T : (static member Pow : ^T * ^T -> ^T) (x, y))
    //     Array.map (fun x -> pow x scalar) v

    // let inline sqrtGeneric<'T when 'T :> INumberRootFunctions<'T>
    //                                and 'T : (new: unit -> 'T)
    //                                and 'T : struct
    //                                and 'T :> ValueType> (v: 'T[]) : 'T[] =
    //     let inline sqrt x = (^T : (static member Sqrt : ^T -> ^T) x)
    //     Array.map sqrt v

    // let inline expGeneric<'T when 'T :> IExponentialFunctions<'T>
    //                               and 'T : (new: unit -> 'T)
    //                               and 'T : struct
    //                               and 'T :> ValueType> (v: 'T[]) : 'T[] =
    //     let inline exp x = (^T : (static member Exp : ^T -> ^T) x)
    //     Array.map exp v
