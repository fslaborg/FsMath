namespace FsMath.Tests.Matrix

open System
open System.Linq.Expressions
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

/// <summary>
/// Quotation-based tests for inline functions in MatrixModule.
/// These tests use F# quotation evaluation to force the coverage tool
/// to track inline function execution.
/// </summary>
module MatrixModuleCoverageTests =

    /// Helper to evaluate F# quotations
    let inline eval<'T> (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        let linq = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression expr
        let lambda = Expression.Lambda<Func<'T>>(linq).Compile()
        lambda.Invoke()

    // ====== copy quotation tests ======

    [<Fact>]
    let ``copy Q: creates a deep copy of a matrix`` () =
        let original = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let copied = eval <@ Matrix.copy original @>

        // Verify values are the same
        Assert.Equal(1.0, copied.[0, 0])
        Assert.Equal(2.0, copied.[0, 1])
        Assert.Equal(3.0, copied.[1, 0])
        Assert.Equal(4.0, copied.[1, 1])

        // Verify it's a deep copy by modifying original
        original.[0, 0] <- 99.0
        Assert.Equal(1.0, copied.[0, 0])  // Copied should remain unchanged

    [<Fact>]
    let ``copy Q: preserves dimensions`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let copied = eval <@ Matrix.copy m @>
        Assert.Equal(m.NumRows, copied.NumRows)
        Assert.Equal(m.NumCols, copied.NumCols)

    [<Fact>]
    let ``copy Q: works with single element matrix`` () =
        let m = Matrix.ofArray2D (array2D [[42]])
        let copied = eval <@ Matrix.copy m @>
        Assert.Equal(42, copied.[0, 0])

    [<Fact>]
    let ``copy Q: works with larger matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]])
        let copied = eval <@ Matrix.copy m @>

        Assert.Equal(3, copied.NumRows)
        Assert.Equal(4, copied.NumCols)
        Assert.Equal(1, copied.[0, 0])
        Assert.Equal(12, copied.[2, 3])

    [<Fact>]
    let ``copy Q: preserves all values correctly`` () =
        let m = Matrix.ofArray2D (array2D [[1.5; 2.5; 3.5]; [4.5; 5.5; 6.5]])
        let copied = eval <@ Matrix.copy m @>

        for i in 0 .. m.NumRows - 1 do
            for j in 0 .. m.NumCols - 1 do
                Assert.Equal(m.[i, j], copied.[i, j])

    // ====== ofRowSeq quotation tests ======

    [<Fact>]
    let ``ofRowSeq Q: creates matrix from sequence of sequences`` () =
        let rows = seq { yield seq [1; 2]; yield seq [3; 4] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(2, m.NumRows)
        Assert.Equal(2, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(4, m.[1, 1])

    [<Fact>]
    let ``ofRowSeq Q: throws on empty sequence`` () =
        let rows = Seq.empty<seq<int>>
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.ofRowSeq rows @> |> ignore)

    [<Fact>]
    let ``ofRowSeq Q: throws on mismatched row lengths`` () =
        let rows = seq { yield seq [1; 2]; yield seq [3; 4; 5] }
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.ofRowSeq rows @> |> ignore)

    [<Fact>]
    let ``ofRowSeq Q: handles single element per row`` () =
        let rows = seq { yield seq [1]; yield seq [2]; yield seq [3] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(3, m.NumRows)
        Assert.Equal(1, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(2, m.[1, 0])
        Assert.Equal(3, m.[2, 0])

    [<Fact>]
    let ``ofRowSeq Q: handles single row`` () =
        let rows = seq { yield seq [10; 20; 30; 40] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(1, m.NumRows)
        Assert.Equal(4, m.NumCols)
        Assert.Equal(10, m.[0, 0])
        Assert.Equal(40, m.[0, 3])

    [<Fact>]
    let ``ofRowSeq Q: works with float sequences`` () =
        let rows = seq { yield seq [1.0; 2.0; 3.0]; yield seq [4.0; 5.0; 6.0] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(2, m.NumRows)
        Assert.Equal(3, m.NumCols)
        floatEqual 1.0 m.[0, 0] 1e-10
        floatEqual 6.0 m.[1, 2] 1e-10

    [<Fact>]
    let ``ofRowSeq Q: works with arrays as sequences`` () =
        let rows = seq { yield [|1; 2; 3|]; yield [|4; 5; 6|] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(2, m.NumRows)
        Assert.Equal(3, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(6, m.[1, 2])

    [<Fact>]
    let ``ofRowSeq Q: works with lists as sequences`` () =
        let rows = seq { yield [1; 2]; yield [3; 4]; yield [5; 6] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(3, m.NumRows)
        Assert.Equal(2, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(6, m.[2, 1])

    [<Fact>]
    let ``ofRowSeq Q: works with larger matrix`` () =
        let rows = seq {
            yield seq [1; 2; 3; 4; 5]
            yield seq [6; 7; 8; 9; 10]
            yield seq [11; 12; 13; 14; 15]
        }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(3, m.NumRows)
        Assert.Equal(5, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(15, m.[2, 4])
        Assert.Equal(8, m.[1, 2])

    [<Fact>]
    let ``ofRowSeq Q: first empty row causes validation failure`` () =
        // First row is empty (0 columns), second row has 2 elements - should fail validation
        let rows = seq { yield Seq.empty<int>; yield seq [1; 2] }
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.ofRowSeq rows @> |> ignore)

    [<Fact>]
    let ``ofRowSeq Q: detects mismatched row at second position`` () =
        let rows = seq { yield seq [1; 2; 3]; yield seq [4; 5] }
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.ofRowSeq rows @> |> ignore)

    [<Fact>]
    let ``ofRowSeq Q: detects mismatched row at last position`` () =
        let rows = seq { yield seq [1; 2]; yield seq [3; 4]; yield seq [5; 6; 7] }
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.ofRowSeq rows @> |> ignore)

    [<Fact>]
    let ``ofRowSeq Q: all rows same length validates correctly`` () =
        let rows = seq {
            yield seq [1; 2; 3]
            yield seq [4; 5; 6]
            yield seq [7; 8; 9]
            yield seq [10; 11; 12]
        }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(4, m.NumRows)
        Assert.Equal(3, m.NumCols)

    [<Fact>]
    let ``ofRowSeq Q: preserves element order in row-major layout`` () =
        let rows = seq { yield seq [1; 2; 3]; yield seq [4; 5; 6] }
        let m = eval <@ Matrix.ofRowSeq rows @>

        // Verify row-major order
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(2, m.[0, 1])
        Assert.Equal(3, m.[0, 2])
        Assert.Equal(4, m.[1, 0])
        Assert.Equal(5, m.[1, 1])
        Assert.Equal(6, m.[1, 2])

    [<Fact>]
    let ``ofRowSeq Q: works with sequence from computation expression`` () =
        let rows = seq {
            for i in 1..3 do
                yield seq { for j in 1..4 -> i * 10 + j }
        }
        let m = eval <@ Matrix.ofRowSeq rows @>

        Assert.Equal(3, m.NumRows)
        Assert.Equal(4, m.NumCols)
        Assert.Equal(11, m.[0, 0])
        Assert.Equal(14, m.[0, 3])
        Assert.Equal(31, m.[2, 0])
        Assert.Equal(34, m.[2, 3])
