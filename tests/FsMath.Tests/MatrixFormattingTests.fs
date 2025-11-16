namespace FsMath.Tests.Matrix

open System
open Xunit
open FsMath

/// Tests for Matrix toFormattedString and formatCell functionality
/// These tests ensure that formatCell correctly handles all numeric types
module MatrixFormattingTests =

    [<Fact>]
    let ``toFormattedString with float matrix`` () =
        let mat = matrix [| [|1.5; 2.75|]
                            [|3.25; 4.125|] |]
        let str = mat.toFormattedString()
        Assert.Contains("Matrix", str)
        Assert.Contains("1.5", str)
        Assert.Contains("2.75", str)

    [<Fact>]
    let ``toFormattedString with float32 matrix`` () =
        let mat = Matrix.create 2 2 [|1.5f; 2.75f; 3.25f; 4.125f|]
        let str = mat.toFormattedString()
        Assert.Contains("Matrix", str)
        Assert.Contains("1.5", str)
        Assert.Contains("2.75", str)

    [<Fact>]
    let ``toFormattedString with int matrix`` () =
        let mat = Matrix.create 2 2 [|1; 2; 3; 4|]
        let str = mat.toFormattedString()
        Assert.Contains("Matrix", str)
        Assert.Contains("1", str)
        Assert.Contains("4", str)

    [<Fact>]
    let ``toFormattedString with custom float format`` () =
        let mat = matrix [| [|1.123456; 2.789012|]
                            [|3.456789; 4.987654|] |]
        let str = mat.toFormattedString(floatFormat = "F2")
        Assert.Contains("1.12", str)
        Assert.Contains("2.79", str)

    [<Fact>]
    let ``toFormattedString with scientific notation`` () =
        let mat = matrix [| [|0.00123; 0.00456|]
                            [|0.00789; 0.00012|] |]
        let str = mat.toFormattedString(useScientific = true)
        Assert.Contains("E", str)

    [<Fact>]
    let ``toFormattedString with maxRows truncation`` () =
        let mat = Matrix.init 10 3 (fun r c -> float (r * 3 + c))
        let str = mat.toFormattedString(maxRows = 5)
        Assert.Contains("truncated", str)
        Assert.Contains("10x3", str)

    [<Fact>]
    let ``toFormattedString with maxCols truncation`` () =
        let mat = Matrix.init 3 10 (fun r c -> float (r * 10 + c))
        let str = mat.toFormattedString(maxCols = 5)
        Assert.Contains("truncated", str)
        Assert.Contains("3x10", str)

    [<Fact>]
    let ``toFormattedString handles negative floats`` () =
        let mat = matrix [| [|-1.5; -2.75|]
                            [|-3.25; -4.125|] |]
        let str = mat.toFormattedString()
        Assert.Contains("-1.5", str)
        Assert.Contains("-2.75", str)

    [<Fact>]
    let ``toFormattedString handles negative float32`` () =
        let mat = Matrix.create 2 2 [|-1.5f; -2.75f; -3.25f; -4.125f|]
        let str = mat.toFormattedString()
        Assert.Contains("-1.5", str)
        Assert.Contains("-2.75", str)

    [<Fact>]
    let ``toFormattedString handles mixed positive and negative floats`` () =
        let mat = matrix [| [|1.5; -2.75|]
                            [|-3.25; 4.12|] |]
        let str = mat.toFormattedString()
        Assert.Contains("1.5", str)
        Assert.Contains("-2.75", str)
        Assert.Contains("-3.25", str)
        Assert.Contains("4.12", str)

    [<Fact>]
    let ``toFormattedString handles zero in float matrix`` () =
        let mat = matrix [| [|0.0; 1.0|]
                            [|2.0; 0.0|] |]
        let str = mat.toFormattedString()
        Assert.Contains("0", str)

    [<Fact>]
    let ``toFormattedString handles zero in float32 matrix`` () =
        let mat = Matrix.create 2 2 [|0.0f; 1.0f; 2.0f; 0.0f|]
        let str = mat.toFormattedString()
        Assert.Contains("0", str)

    [<Fact>]
    let ``toFormattedString handles large integers`` () =
        let mat = Matrix.create 2 2 [|1000; 2000; 3000; 4000|]
        let str = mat.toFormattedString()
        Assert.Contains("1000", str)
        Assert.Contains("4000", str)

    [<Fact>]
    let ``toFormattedString handles very small float32 values`` () =
        let mat = Matrix.create 2 2 [|0.001f; 0.002f; 0.003f; 0.004f|]
        let str = mat.toFormattedString()
        Assert.Contains("Matrix", str)

    [<Fact>]
    let ``toFormattedString handles very large float32 values`` () =
        let mat = Matrix.create 2 2 [|1000.0f; 2000.0f; 3000.0f; 4000.0f|]
        let str = mat.toFormattedString()
        Assert.Contains("1000", str)
        Assert.Contains("4000", str)

    [<Fact>]
    let ``toFormattedString with float32 and scientific notation`` () =
        let mat = Matrix.create 2 2 [|0.00123f; 0.00456f; 0.00789f; 0.00012f|]
        let str = mat.toFormattedString(useScientific = true)
        Assert.Contains("E", str)

    [<Fact>]
    let ``toFormattedString with single row float32 matrix`` () =
        let mat = Matrix.create 1 3 [|1.5f; 2.5f; 3.5f|]
        let str = mat.toFormattedString()
        Assert.Contains("Matrix 1x3", str)
        Assert.Contains("1.5", str)

    [<Fact>]
    let ``toFormattedString with single column float32 matrix`` () =
        let mat = Matrix.create 3 1 [|1.5f; 2.5f; 3.5f|]
        let str = mat.toFormattedString()
        Assert.Contains("Matrix 3x1", str)
        Assert.Contains("1.5", str)

    [<Fact>]
    let ``toFormattedString with int64 matrix`` () =
        let mat = Matrix.create 2 2 [|100L; 200L; 300L; 400L|]
        let str = mat.toFormattedString()
        Assert.Contains("100", str)
        Assert.Contains("400", str)

    [<Fact>]
    let ``toFormattedString handles alternating floats and float32s in pattern`` () =
        // Test both float and float32 formatting paths
        let matFloat = matrix [| [|1.234; 5.678|] |]
        let matFloat32 = Matrix.create 1 2 [|1.234f; 5.678f|]

        let strFloat = matFloat.toFormattedString()
        let strFloat32 = matFloat32.toFormattedString()

        Assert.Contains("1.23", strFloat)
        Assert.Contains("1.23", strFloat32)
