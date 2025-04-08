namespace FsMath

open System
open System.Runtime.InteropServices


/// <summary>
/// A utility class for performing SIMD (Single Instruction, Multiple Data) operations on arrays.
/// </summary>
type SIMDUtils() =

    /// <summary>
    /// Applies a safe element-wise unary operation on an array using SIMD for performance.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD operation on a vector.</param>
    /// <param name="f">A fallback function for scalar operations.</param>
    /// <param name="v">The input array.</param>
    /// <returns>An array with the operation applied to each element.</returns>
    static member inline map<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)                              
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (f: 'T -> 'T)
                (v: 'T[]) : 'T[] =

        let length = v.Length
        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let results = Array.zeroCreate<'T> length
        let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v.AsSpan())

        for i = 0 to slotCount - 1 do
            rsSpan.[i] <- fv vSpan[i]

        for i = ceiling to length - 1 do
            results.[i] <- f v.[i]

        results

    /// <summary>
    /// Applies an element-wise binary operation on two arrays using SIMD for performance.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD operation on two vectors.</param>
    /// <param name="f">A fallback function for scalar operations.</param>
    /// <param name="v1">The first input array.</param>
    /// <param name="v2">The second input array.</param>
    /// <returns>An array with the operation applied to each pair of elements.</returns>
    static member inline map2Unchecked<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>) 
                (f:'T -> 'T -> 'T) 
                (v1: 'T[]) 
                (v2: 'T[])  =

        let length = v1.Length
        let slotSize= Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let results = Array.zeroCreate<'T> length
        let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
        let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
        let v2Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v2.AsSpan())

        for i=0 to slotCount - 1 do
            rsSpan.[i] <- fv v1Span[i] v2Span[i]

        for i=ceiling to v1.Length - 1 do
            results.[i] <- f v1.[i] v2[i]

        results

    /// <summary>
    /// Reduces an array to a single value using a binary operation, with SIMD optimization.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD reduction on two vectors.</param>
    /// <param name="f">A fallback function for scalar reduction.</param>
    /// <param name="zero">The initial value for the reduction.</param>
    /// <param name="v">The input array.</param>
    /// <returns>The result of the reduction.</returns>
    static member inline fold<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (f: 'T -> 'T -> 'T)
                (zero: 'T)
                (v: 'T[]) : 'T =

        let length = v.Length
        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let vSpan: Span<Numerics.Vector<'T>> = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v.AsSpan())

        let mutable accVec = Numerics.Vector<'T>(zero)

        for i = 0 to slotCount - 1 do
            accVec <- fv accVec vSpan[i]

        let mutable acc = accVec.[0]
        for i = 1 to slotSize - 1 do
            acc <- f acc accVec.[i]

        for i = ceiling to length - 1 do
            acc <- f acc v[i]

        acc

    /// <summary>
    /// Applies an element-wise binary operation between an array and a scalar using SIMD for performance.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD operation on a vector and a scalar vector.</param>
    /// <param name="f">A fallback function for scalar operations.</param>
    /// <param name="v1">The input array.</param>
    /// <param name="scalar">The scalar value.</param>
    /// <returns>An array with the operation applied to each element and the scalar.</returns>
    static member inline mapScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> ) 
                (f:'T -> 'T -> 'T) 
                (v1:'T[]) 
                (scalar:'T) : 'T[] =
        
        let length = v1.Length
        let slotSize= Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let results = Array.zeroCreate<'T> length
        let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
        let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
        let scalarVec = Numerics.Vector(scalar)

        for i=0 to slotCount - 1 do
            rsSpan.[i] <- fv v1Span[i] scalarVec

        for i=ceiling to v1.Length - 1 do
            results.[i] <- f v1.[i] scalar

        results


// type SIMDMatrixUtils() =

//     // 
//     static member map2Unchecked<'T when 'T :> Numerics.INumber<'T>
//                 and 'T : (new: unit -> 'T)
//                 and 'T : struct
//                 and 'T :> ValueType>
//                 (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>) 
//                 (f:'T -> 'T -> 'T) 
//                 (v1: 'T[]) 
//                 (v2: 'T[]) : 'T[] =
        

//         // Gets the total number of elements of this vector
//         let length = v1.Length
//         // 
//         let slotSize= Numerics.Vector<'T>.Count
//         // Number of slots (rasterized vectors)
//         let slotCount = length / slotSize
//         // Index at which the non rasterd remain starts
//         let ceiling = slotSize * slotCount

//         // Allocate the result array
//         let results = Array.zeroCreate<'T> length
//         let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
//         let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
//         let v2Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v2.AsSpan())

//         // Perform SIMD addition for chunks
//         for i=0 to slotCount - 1 do
//             rsSpan.[i] <- fv v1Span[i] v2Span[i]

//         // Handle the remaining elements
//         for i=ceiling to v1.Length - 1 do
//             results.[i] <- f v1.[i] v2[i]

//         results

// type SIMDMatrixUtils() =

//     static member map2<'T when 'T :> Numerics.INumber<'T>
//                 and 'T : (new: unit -> 'T)
//                 and 'T : struct
//                 and 'T :> ValueType> 
//                 (fVec: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
//                 (fScalar: 'T -> 'T -> 'T)
//                 (a: Matrix<'T>)
//                 (b: Matrix<'T>)
//                 : Matrix<'T> =

//         if a.Rows <> b.Rows || a.Cols <> b.Cols then
//             invalidArg "b" "Matrix dimensions must match"

//         let len = a.Data.Length
//         let result = Array.zeroCreate<'T> len

//         let simdSize = Numerics.Vector<'T>.Count
//         let simdCount = len / simdSize
//         let tailStart = simdCount * simdSize

//         // Cast arrays to Vector<'T> spans
//         let aVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(a.Data.AsSpan())
//         let bVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(b.Data.AsSpan())
//         let rVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

//         for i = 0 to simdCount - 1 do
//             rVec.[i] <- fVec aVec.[i] bVec.[i]

//         for i = tailStart to len - 1 do
//             result.[i] <- fScalar a.Data.[i] b.Data.[i]

//         Matrix(a.Rows, a.Cols, result)


//     static member mapScalar<'T when 'T :> Numerics.INumber<'T>
//                 and 'T : (new: unit -> 'T)
//                 and 'T : struct
//                 and 'T :> ValueType> 
//                 (fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
//                 (f: 'T -> 'T -> 'T)
//                 (matrix: Matrix<'T>)
//                 (scalar: 'T)
//                 : Matrix<'T> =

//         let length = matrix.Data.Length
//         let result = Array.zeroCreate<'T> length

//         let slotSize = Numerics.Vector<'T>.Count
//         let slotCount = length / slotSize
//         let ceiling = slotCount * slotSize

//         let scalarVec = Numerics.Vector<'T>(scalar)

//         let mSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(matrix.Data.AsSpan())
//         let rSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

//         // SIMD chunk-wise operation
//         for i = 0 to slotCount - 1 do
//             rSpan.[i] <- fv mSpan.[i] scalarVec

//         // Scalar fallback for remainder
//         for i = ceiling to length - 1 do
//             result.[i] <- f matrix.Data.[i] scalar

//         Matrix(matrix.Rows, matrix.Cols, result)
