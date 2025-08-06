namespace FsMath

open FsMath
open System
open System.Runtime.InteropServices

/// Vector as a Array type alias
type Vector<'T when 'T :> Numerics.INumber<'T>> = 'T []

type Vector =
    
    /// Creates a new vector with the specified length, initialized to zero.
    static member inline zeroCreate<'T when 'T :> Numerics.INumber<'T>> count : Vector<'T> =
        Array.zeroCreate<'T> count

    /// Adds two vectors element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise sum of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>    
    static member inline add (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        // ReadOnlySpan<'T>(array, offset, length)
        SpanMath.add(ReadOnlySpan<'T>(v1), ReadOnlySpan<'T>(v2))


    /// Subtracts the second vector from the first vector element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise difference of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception> 
    static member inline subtract (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        SpanMath.subtract(ReadOnlySpan<'T>(v1), ReadOnlySpan<'T>(v2))      
        

    /// Multiplies two vectors element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise product of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>
    static member inline multiply (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        SpanMath.multiply(ReadOnlySpan<'T>(v1), ReadOnlySpan<'T>(v2))
        
        

    /// Divides the first vector by the second vector element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise division of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>
    static member inline divide (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        SpanMath.divide(ReadOnlySpan<'T>(v1), ReadOnlySpan<'T>(v2))

        

    /// Adds a scalar to each element of the vector.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to add.</param>
    /// <returns>A new vector with the scalar added to each element.</returns>
    static member inline addScalar (scalar:'T) (v:Vector<'T>) : Vector<'T> =
        SpanMath.addScalar(ReadOnlySpan<'T>(v), scalar)        
        

    /// Subtracts a scalar from each element of the vector.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to subtract.</param>
    /// <returns>A new vector with the scalar subtracted from each element.</returns>
    static member inline subtractScalar (scalar:'T) (v:Vector<'T>) : Vector<'T> =
         SpanMath.subtractScalar(ReadOnlySpan<'T>(v), scalar) 
        

    /// Multiplies each element of the vector by a scalar.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to multiply by.</param>
    /// <returns>A new vector with each element multiplied by the scalar.</returns>
    static member inline multiplyScalar (scalar:'T) (v:Vector<'T>) : Vector<'T> =
         SpanMath.multiplyScalar(ReadOnlySpan<'T>(v), scalar)       
        

    /// Divides each element of the vector by a scalar.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to divide by.</param>
    /// <returns>A new vector with each element divided by the scalar.</returns>
    static member inline divideScalar (scalar:'T) (v:Vector<'T>) : Vector<'T> =
         SpanMath.multiplyScalar(ReadOnlySpan<'T>(v), scalar)

    /// Computes the sum of all elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The sum of all elements in the vector.</returns>
    static member inline sum (v:Vector<'T>) : 'T =
        SpanMath.sum(ReadOnlySpan<'T>(v))

    /// Computes the product of all elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The product of all elements in the vector.</returns>
    static member inline product (v:Vector<'T>) : 'T =
        SpanMath.product(ReadOnlySpan<'T>(v))
        

    /// Computes the mean of the elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The mean of the elements in the vector.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vector is empty.</exception>
    static member inline mean (v: Vector<'T>) : 'T =
        SpanMath.mean(ReadOnlySpan<'T>(v))

    /// Computes the dot product ab^T of two vectors. 
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>The dot product of the two vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>
    static member inline dot (v1 : Vector<'T>) (v2 : Vector<'T>) : 'T =
        SpanMath.dot(ReadOnlySpan<'T>(v1), ReadOnlySpan<'T>(v2))


    /// Computes the Euclidean norm (magnitude) of the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The Euclidean norm of the vector.</returns>
    static member inline norm (v:Vector<'T>) : 'T =
        SpanMath.norm(ReadOnlySpan<'T>(v))


    /// Finds the index of the minimum value in the vector.
    static member inline min (v:Vector<'T>) : 'T =
        SpanMath.min(ReadOnlySpan<'T>(v))
    

    /// Finds the index of the maximum value in the vector.
    static member inline max (v:Vector<'T>) : 'T =
        SpanMath.max(ReadOnlySpan<'T>(v))



