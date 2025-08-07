namespace FsMath.Tests

open Xunit
open FsMath
open ExpectoStyle


module AssertHelpers =

    let throws<'exn when 'exn :> exn> (f: unit -> unit) =
        Assert.Throws<'exn>(System.Action f) |> ignore

    let floatEqual (expected: float) (actual: float) (eps: float) =
        Assert.InRange(actual, expected - eps, expected + eps)

    let floatArrayClose (expected: float[]) (actual: float[]) (eps: float) =
        Assert.Equal(expected.Length, actual.Length)
        Array.iter2 (fun e a -> Assert.InRange(a, e - eps, e + eps)) expected actual

    let intEqual (expected: int) (actual: int) =
        Assert.Equal(expected, actual)

    let intArrayEqual (expected: int[]) (actual: int[]) =
        Assert.Equal(expected.Length, actual.Length)
        Assert.Equal<seq<int>>(expected, actual)

    let VectorEqual (expected: Vector<'T>) (actual: Vector<'T>) =
        Assert.Equal(expected.Length, actual.Length)
        Assert.Equal<Vector<'T>>(expected, actual)

    let floatMatrixClose (accuracy: Accuracy) (expected: Matrix<float>) (actual: Matrix<float>) =
        Assert.Equal(expected.NumRows, actual.NumRows)
        Assert.Equal(expected.NumCols, actual.NumCols)
        floatArrayClose expected.Data actual.Data accuracy.absolute    
        
    let inRangeMatrixValues (accuracy: Accuracy) (expected: Matrix<float>) (actual: Matrix<float>)  =
        let eps = accuracy.absolute
        Assert.Equal(expected.NumRows, actual.NumRows)
        Assert.Equal(expected.NumCols, actual.NumCols)
        for r in 0 .. expected.NumRows - 1 do
            for c in 0 .. expected.NumCols - 1 do
                let expVal = expected.[r, c]
                let actVal = actual.[r, c]
                Assert.InRange(actVal, expVal - eps, expVal + eps)

