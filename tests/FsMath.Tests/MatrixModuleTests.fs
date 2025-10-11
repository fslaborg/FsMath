namespace FsMath.Tests.Matrix

open System
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

module MatrixModuleTests =

    // ====== copy tests ======

    [<Fact>]
    let ``copy creates a deep copy of a matrix`` () =
        let original = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let copied = Matrix.copy original

        // Verify values are the same
        Assert.Equal(1.0, copied.[0, 0])
        Assert.Equal(2.0, copied.[0, 1])
        Assert.Equal(3.0, copied.[1, 0])
        Assert.Equal(4.0, copied.[1, 1])

        // Verify it's a deep copy by modifying original
        original.[0, 0] <- 99.0
        Assert.Equal(1.0, copied.[0, 0])  // Copied should remain unchanged

    [<Fact>]
    let ``copy preserves dimensions`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let copied = Matrix.copy m
        Assert.Equal(m.NumRows, copied.NumRows)
        Assert.Equal(m.NumCols, copied.NumCols)

    [<Fact>]
    let ``copy works with single element matrix`` () =
        let m = Matrix.ofArray2D (array2D [[42]])
        let copied = Matrix.copy m
        Assert.Equal(42, copied.[0, 0])

    // ====== splitRows tests ======

    [<Fact>]
    let ``splitRows splits matrix by row indices`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]; [5; 6]; [7; 8]])
        let (selected, rest) = Matrix.splitRows [|1; 3|] m

        // Selected should have rows 1 and 3
        Assert.Equal(2, selected.NumRows)
        Assert.Equal(2, selected.NumCols)
        Assert.Equal(3, selected.[0, 0])
        Assert.Equal(4, selected.[0, 1])
        Assert.Equal(7, selected.[1, 0])
        Assert.Equal(8, selected.[1, 1])

        // Rest should have rows 0 and 2
        Assert.Equal(2, rest.NumRows)
        Assert.Equal(1, rest.[0, 0])
        Assert.Equal(5, rest.[1, 0])

    [<Fact>]
    let ``splitRows handles single row selection`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]; [5; 6]])
        let (selected, rest) = Matrix.splitRows [|1|] m

        Assert.Equal(1, selected.NumRows)
        Assert.Equal(3, selected.[0, 0])
        Assert.Equal(2, rest.NumRows)

    [<Fact>]
    let ``splitRows handles unsorted indices`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]; [5; 6]])
        let (selected, rest) = Matrix.splitRows [|2; 0|] m

        // Indices should be sorted internally, so [0, 2]
        Assert.Equal(2, selected.NumRows)
        Assert.Equal(1, selected.[0, 0])  // Row 0
        Assert.Equal(5, selected.[1, 0])  // Row 2

    [<Fact>]
    let ``splitRows works with all rows selected`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let (selected, rest) = Matrix.splitRows [|0; 1|] m

        Assert.Equal(2, selected.NumRows)
        Assert.Equal(0, rest.NumRows)  // No rows left

    // ====== splitCols tests ======

    [<Fact>]
    let ``splitCols splits matrix by column indices`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3; 4]; [5; 6; 7; 8]])
        let (selected, rest) = Matrix.splitCols [|1; 3|] m

        // Selected should have cols 1 and 3
        Assert.Equal(2, selected.NumRows)
        Assert.Equal(2, selected.NumCols)
        Assert.Equal(2, selected.[0, 0])
        Assert.Equal(4, selected.[0, 1])
        Assert.Equal(6, selected.[1, 0])
        Assert.Equal(8, selected.[1, 1])

        // Rest should have cols 0 and 2
        Assert.Equal(2, rest.NumCols)
        Assert.Equal(1, rest.[0, 0])
        Assert.Equal(3, rest.[0, 1])

    [<Fact>]
    let ``splitCols handles single column selection`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let (selected, rest) = Matrix.splitCols [|1|] m

        Assert.Equal(1, selected.NumCols)
        Assert.Equal(2, selected.[0, 0])
        Assert.Equal(2, rest.NumCols)

    // ====== permuteRowsBy tests ======

    [<Fact>]
    let ``permuteRowsBy reorders rows according to permutation`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]; [5; 6]])
        let P = Permutation.ofFreshArray [|2; 0; 1|]  // Row 0->2, 1->0, 2->1
        let result = Matrix.permuteRowsBy P m

        Assert.Equal(5, result.[0, 0])  // Original row 2
        Assert.Equal(6, result.[0, 1])
        Assert.Equal(1, result.[1, 0])  // Original row 0
        Assert.Equal(2, result.[1, 1])
        Assert.Equal(3, result.[2, 0])  // Original row 1
        Assert.Equal(4, result.[2, 1])

    [<Fact>]
    let ``permuteRowsBy with identity permutation preserves matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let P = Permutation.identity
        let result = Matrix.permuteRowsBy P m

        Assert.Equal(1, result.[0, 0])
        Assert.Equal(2, result.[0, 1])
        Assert.Equal(3, result.[1, 0])
        Assert.Equal(4, result.[1, 1])

    [<Fact>]
    let ``permuteRowsBy works with single row matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]])
        let P = Permutation.identity
        let result = Matrix.permuteRowsBy P m

        Assert.Equal(1, result.[0, 0])
        Assert.Equal(2, result.[0, 1])
        Assert.Equal(3, result.[0, 2])

    // ====== ofRows tests ======

    [<Fact>]
    let ``ofRows creates matrix from array of arrays`` () =
        let rows = [|[|1; 2; 3|]; [|4; 5; 6|]|]
        let m = Matrix.ofRows rows

        Assert.Equal(2, m.NumRows)
        Assert.Equal(3, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(6, m.[1, 2])

    [<Fact>]
    let ``ofRows works with single row`` () =
        let rows = [|[|1; 2; 3|]|]
        let m = Matrix.ofRows rows

        Assert.Equal(1, m.NumRows)
        Assert.Equal(3, m.NumCols)

    // ====== ofRowSeq tests ======

    [<Fact>]
    let ``ofRowSeq creates matrix from sequence of sequences`` () =
        let rows = seq { yield seq [1; 2]; yield seq [3; 4] }
        let m = Matrix.ofRowSeq rows

        Assert.Equal(2, m.NumRows)
        Assert.Equal(2, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(4, m.[1, 1])

    [<Fact>]
    let ``ofRowSeq throws on empty sequence`` () =
        let rows = Seq.empty<seq<int>>
        Assert.Throws<ArgumentException>(fun () -> Matrix.ofRowSeq rows |> ignore)

    [<Fact>]
    let ``ofRowSeq throws on mismatched row lengths`` () =
        let rows = seq { yield seq [1; 2]; yield seq [3; 4; 5] }
        Assert.Throws<ArgumentException>(fun () -> Matrix.ofRowSeq rows |> ignore)

    [<Fact>]
    let ``ofRowSeq handles single element per row`` () =
        let rows = seq { yield seq [1]; yield seq [2]; yield seq [3] }
        let m = Matrix.ofRowSeq rows

        Assert.Equal(3, m.NumRows)
        Assert.Equal(1, m.NumCols)

    // ====== mapiRows tests ======

    [<Fact>]
    let ``mapiRows applies function to each row with index`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.mapiRows (fun i row -> Array.map (fun x -> x + i) row) m

        Assert.Equal(1, result.[0, 0])  // 1 + 0
        Assert.Equal(2, result.[0, 1])  // 2 + 0
        Assert.Equal(4, result.[1, 0])  // 3 + 1
        Assert.Equal(5, result.[1, 1])  // 4 + 1

    [<Fact>]
    let ``mapiRows can change type`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.mapiRows (fun i row -> Array.map (fun x -> float x * 2.0) row) m

        Assert.Equal(2.0, result.[0, 0])
        Assert.Equal(4.0, result.[0, 1])
        Assert.Equal(6.0, result.[1, 0])
        Assert.Equal(8.0, result.[1, 1])

    // ====== ofCols tests ======

    [<Fact>]
    let ``ofCols creates matrix from array of column arrays`` () =
        let cols = [|[|1; 2|]; [|3; 4|]; [|5; 6|]|]
        let m = Matrix.ofCols cols

        Assert.Equal(2, m.NumRows)
        Assert.Equal(3, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(2, m.[1, 0])
        Assert.Equal(3, m.[0, 1])
        Assert.Equal(4, m.[1, 1])

    [<Fact>]
    let ``ofCols works with single column`` () =
        let cols = [|[|1; 2; 3|]|]
        let m = Matrix.ofCols cols

        Assert.Equal(3, m.NumRows)
        Assert.Equal(1, m.NumCols)

    // ====== mapiCols tests ======

    [<Fact>]
    let ``mapiCols applies function to each column with index`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let result = Matrix.mapiCols (fun j col -> Array.map (fun x -> x + j) col) m

        Assert.Equal(1, result.[0, 0])  // 1 + 0
        Assert.Equal(4, result.[1, 0])  // 4 + 0
        Assert.Equal(3, result.[0, 1])  // 2 + 1
        Assert.Equal(6, result.[1, 1])  // 5 + 1
        Assert.Equal(5, result.[0, 2])  // 3 + 2
        Assert.Equal(8, result.[1, 2])  // 6 + 2

    [<Fact>]
    let ``mapiCols can change type`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.mapiCols (fun j col -> Array.map (fun x -> float x * 2.0) col) m

        Assert.Equal(2.0, result.[0, 0])
        Assert.Equal(6.0, result.[1, 0])

    // ====== mapi tests ======

    [<Fact>]
    let ``mapi applies function with row and column indices`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.mapi (fun i j x -> x + i + j) m

        Assert.Equal(1, result.[0, 0])  // 1 + 0 + 0
        Assert.Equal(3, result.[0, 1])  // 2 + 0 + 1
        Assert.Equal(4, result.[1, 0])  // 3 + 1 + 0
        Assert.Equal(6, result.[1, 1])  // 4 + 1 + 1

    [<Fact>]
    let ``mapi can change element type`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.mapi (fun i j x -> float (x + i + j)) m

        Assert.Equal(1.0, result.[0, 0])
        Assert.Equal(3.0, result.[0, 1])
        Assert.Equal(4.0, result.[1, 0])
        Assert.Equal(6.0, result.[1, 1])

    [<Fact>]
    let ``mapi processes in row-major order`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let mutable count = 0
        let result =
            Matrix.mapi (fun i j x ->
                count <- count + 1
                count) m

        // Verify processing happens in row-major order
        Assert.Equal(1, result.[0, 0])
        Assert.Equal(2, result.[0, 1])
        Assert.Equal(3, result.[0, 2])
        Assert.Equal(4, result.[1, 0])

    // ====== map tests ======

    [<Fact>]
    let ``map applies function to all elements`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.map (fun x -> x * 2) m

        Assert.Equal(2, result.[0, 0])
        Assert.Equal(4, result.[0, 1])
        Assert.Equal(6, result.[1, 0])
        Assert.Equal(8, result.[1, 1])

    [<Fact>]
    let ``map can change element type`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let result = Matrix.map (fun x -> int x) m

        Assert.Equal(1, result.[0, 0])
        Assert.Equal(4, result.[1, 1])

    [<Fact>]
    let ``map preserves dimensions`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let result = Matrix.map (fun x -> x + 1) m

        Assert.Equal(2, result.NumRows)
        Assert.Equal(3, result.NumCols)

    // ====== foldi tests ======

    [<Fact>]
    let ``foldi accumulates over matrix with indices`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.foldi (fun i j acc x -> acc + i + j + x) 0 m

        // (0+0+1) + (0+1+2) + (1+0+3) + (1+1+4) = 1 + 3 + 4 + 6 = 14
        Assert.Equal(14, result)

    [<Fact>]
    let ``foldi processes in row-major order`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.foldi (fun i j acc x -> (i, j, x) :: acc) [] m

        let expected = [(1, 1, 4); (1, 0, 3); (0, 1, 2); (0, 0, 1)]
        Assert.Equal<list<int*int*int>>(expected, result)

    [<Fact>]
    let ``foldi works with single element matrix`` () =
        let m = Matrix.ofArray2D (array2D [[42]])
        let result = Matrix.foldi (fun i j acc x -> acc + x) 0 m

        Assert.Equal(42, result)

    [<Fact>]
    let ``foldi can compute sum`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let result = Matrix.foldi (fun i j acc x -> acc + x) 0 m

        Assert.Equal(21, result)

    // ====== sum tests ======

    [<Fact>]
    let ``sum computes sum of all elements`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = Matrix.sum m

        Assert.Equal(10, result)

    [<Fact>]
    let ``sum works with float matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1.5; 2.5]; [3.0; 4.0]])
        let result = Matrix.sum m

        Assert.Equal(11.0, result)

    [<Fact>]
    let ``sum works with single element`` () =
        let m = Matrix.ofArray2D (array2D [[42]])
        let result = Matrix.sum m

        Assert.Equal(42, result)

    [<Fact>]
    let ``sum works with negative values`` () =
        let m = Matrix.ofArray2D (array2D [[-1; 2]; [3; -4]])
        let result = Matrix.sum m

        Assert.Equal(0, result)
