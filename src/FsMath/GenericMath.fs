namespace FsMath

open System
open System.Runtime.InteropServices

module GenericMath =

    // let inline zero<'T when 'T :> Numerics.INumber<'T>> = LanguagePrimitives.GenericZero<'T>
    // let inline one<'T when 'T :> Numerics.INumber<'T>> = LanguagePrimitives.GenericOne<'T>

    let inline abs<'T when 'T :> Numerics.INumber<'T>> (x: 'T) =
        'T.Abs(x)

    let inline sqrt<'T when 'T :> Numerics.IRootFunctions<'T>> (x: 'T) =
        'T.Sqrt(x)

    let inline exp<'T when 'T :> Numerics.IExponentialFunctions<'T>> (x: 'T) =
        'T.Exp(x)

    // let inline pow<'T when 'T :> Numerics.INumber<'T>> (x: 'T) (y: 'T) =
    //     //'T.Pow(x,y)
    //     ('T : (static member Pow : 'T * 'T -> 'T) (x, y))

    // let inline min x y = if x < y then x else y
    // let inline max x y = if x > y then x else y

    // let inline clamp minVal maxVal x =
    //     if x < minVal then minVal elif x > maxVal then maxVal else x

