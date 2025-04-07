namespace FsMath

open FsMath
open System
open System.Runtime.InteropServices

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =

    /// Indexed fold over a vector
    let inline foldi<'T when 'T :> Numerics.INumber<'T>> f (state: 'T) (v: Vector<'T>) : 'T =
        let mutable acc = state
        for i = 0 to v.Length - 1 do
            acc <- f i acc v.[i]
        acc

    /// Indexed map over a vector
    let inline mapi<'T when 'T :> Numerics.INumber<'T>> (f: int -> 'T -> 'T) (v: Vector<'T>) : Vector<'T> =
        Array.mapi f v

    /// Filter vector elements by predicate
    let inline filter<'T when 'T :> Numerics.INumber<'T>> (predicate: 'T -> bool) (v: Vector<'T>) : Vector<'T> =
        Array.filter predicate v

    /// Initialize vector by index-based generator 
    let inline init<'T when 'T :> Numerics.INumber<'T>> (n: int) (f: int -> 'T) : Vector<'T> =
        Array.init n f

    /// Extract a slice from a vector
    let inline slice<'T when 'T :> Numerics.INumber<'T>> (start: int) (length: int) (v: Vector<'T>) : Vector<'T> =
        Array.sub v start length

    /// Find index of maximum value
    let inline argmax<'T when 'T :> Numerics.INumber<'T> and 'T : comparison> (v: Vector<'T>) : int =
        let mutable maxIdx = 0
        let mutable maxVal = v.[0]
        for i = 1 to v.Length - 1 do
            if v.[i] > maxVal then
                maxVal <- v.[i]
                maxIdx <- i
        maxIdx

    /// Find index of minimum value
    let inline argmin<'T when 'T :> Numerics.INumber<'T> and 'T : comparison> (v: Vector<'T>) : int =
        let mutable minIdx = 0
        let mutable minVal = v.[0]
        for i = 1 to v.Length - 1 do
            if v.[i] < minVal then
                minVal <- v.[i]
                minIdx <- i
        minIdx

    /// Pad vector to a given length with a constant value
    let inline padRight<'T when 'T :> Numerics.INumber<'T>> (length: int) (value: 'T) (v: Vector<'T>) : Vector<'T> =
        if v.Length >= length then v
        else
            Array.init length (fun i -> if i < v.Length then v.[i] else value)

    /// Zip two vectors into (index, left, right)
    let inline zip<'T when 'T :> Numerics.INumber<'T>> (v1: Vector<'T>) (v2: Vector<'T>) : (int * 'T * 'T)[] =
        Array.init (min v1.Length v2.Length) (fun i -> i, v1.[i], v2.[i])



    /// Find index of minimum value using projection
    let inline argminBy<'T, 'U
        when 'T :> Numerics.INumber<'T> and 'U : comparison>
        (f: 'T -> 'U) (v: Vector<'T>) : int =
        let mutable minIdx = 0
        let mutable minVal = f v.[0]
        for i = 1 to v.Length - 1 do
            let candidate = f v.[i]
            if candidate < minVal then
                minVal <- candidate
                minIdx <- i
        minIdx

    /// Find index of maximum value using projection
    let inline argmaxBy<'T, 'U
        when 'T :> Numerics.INumber<'T> and 'U : comparison>
        (f: 'T -> 'U) (v: Vector<'T>) : int =
        let mutable maxIdx = 0
        let mutable maxVal = f v.[0]
        for i = 1 to v.Length - 1 do
            let candidate = f v.[i]
            if candidate > maxVal then
                maxVal <- candidate
                maxIdx <- i
        maxIdx

    /// Try to find index of first matching element
    let inline tryFindIndex<'T when 'T :> Numerics.INumber<'T>> (predicate: 'T -> bool) (v: Vector<'T>) : int option =
        let mutable i = 0
        let mutable found = false
        while i < v.Length && not found do
            if predicate v.[i] then found <- true
            else i <- i + 1
        if found then Some i else None

    /// Find index of first matching element or throw
    let inline findIndex<'T when 'T :> Numerics.INumber<'T>> (predicate: 'T -> bool) (v: Vector<'T>) : int =
        match tryFindIndex predicate v with
        | Some i -> i
        | None -> failwith "Element not found."

    /// Enumerate non-zero elements with index
    let inline enumerateNonZero<'T when 'T :> Numerics.INumber<'T> and 'T : equality> (v: Vector<'T>) : (int * 'T)[] =
        v
        |> Array.mapi (fun i x -> i, x)
        |> Array.filter (fun (_, x) -> x <> 'T.Zero)

    /// Split a vector into prefix and suffix at a given index
    let inline split<'T when 'T :> Numerics.INumber<'T>> (index: int) (v: Vector<'T>) : Vector<'T> * Vector<'T> =
        Array.sub v 0 index, Array.sub v index (v.Length - index)

    /// Chunk a vector into equally sized pieces (last may be shorter)
    let inline chunk<'T when 'T :> Numerics.INumber<'T>> (chunkSize: int) (v: Vector<'T>) : Vector<'T>[] =
        let len = v.Length
        [| for i in 0 .. chunkSize .. len - 1 ->
            let size = min chunkSize (len - i)
            Array.sub v i size |]

    /// Sliding window over the vector
    let inline windowed<'T when 'T :> Numerics.INumber<'T>> (windowSize: int) (v: Vector<'T>) : Vector<'T>[] =
        if windowSize > v.Length then [||]
        else
            [| for i in 0 .. v.Length - windowSize ->
                Array.sub v i windowSize |]
