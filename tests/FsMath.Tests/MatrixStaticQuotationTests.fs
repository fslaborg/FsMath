namespace FsMath.Tests.Matrix

open System
open System.Linq.Expressions
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

/// <summary>
/// Quotation-based tests for inline static methods in Matrix type.
/// These tests use F# quotation evaluation to force the coverage tool
/// to track inline function execution, as recommended by maintainers.
/// </summary>
module MatrixStaticQuotationTests =

    /// Helper to evaluate F# quotations
    let inline eval<'T> (expr: Microsoft.FSharp.Quotations.Expr<'T>) : 'T =
        let linq = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression expr
        let lambda = Expression.Lambda<Func<'T>>(linq).Compile()
        lambda.Invoke()

    // ====== getCol quotation tests ======

    [<Fact>]
    let ``getCol Q: extracts correct column from matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]])
        let col1 = eval <@ Matrix.getCol 1 m @>

        Assert.Equal(3, col1.Length)
        Assert.Equal(2, col1.[0])
        Assert.Equal(5, col1.[1])
        Assert.Equal(8, col1.[2])

    [<Fact>]
    let ``getCol Q: first column`` () =
        let m = Matrix.ofArray2D (array2D [[10.0; 20.0]; [30.0; 40.0]])
        let col0 = eval <@ Matrix.getCol 0 m @>

        Assert.Equal(2, col0.Length)
        floatEqual 10.0 col0.[0] 1e-10
        floatEqual 30.0 col0.[1] 1e-10

    [<Fact>]
    let ``getCol Q: last column`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let col2 = eval <@ Matrix.getCol 2 m @>

        Assert.Equal(2, col2.Length)
        Assert.Equal(3, col2.[0])
        Assert.Equal(6, col2.[1])

    [<Fact>]
    let ``getCol Q: throws on negative index`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.getCol -1 m @> |> ignore)

    [<Fact>]
    let ``getCol Q: throws on index out of bounds`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.getCol 3 m @> |> ignore)

    // ====== getCols quotation tests ======

    [<Fact>]
    let ``getCols Q: extracts all columns`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let cols = eval <@ Matrix.getCols m @>

        Assert.Equal(3, cols.Length)
        Assert.Equal(2, cols.[0].Length)
        Assert.Equal(1, cols.[0].[0])
        Assert.Equal(4, cols.[0].[1])
        Assert.Equal(6, cols.[2].[1])

    [<Fact>]
    let ``getCols Q: single column matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1]; [2]; [3]])
        let cols = eval <@ Matrix.getCols m @>

        Assert.Equal(1, cols.Length)
        Assert.Equal(3, cols.[0].Length)
        Assert.Equal(1, cols.[0].[0])
        Assert.Equal(3, cols.[0].[2])

    [<Fact>]
    let ``getCols Q: single row matrix`` () =
        let m = Matrix.ofArray2D (array2D [[5.0; 10.0; 15.0]])
        let cols = eval <@ Matrix.getCols m @>

        Assert.Equal(3, cols.Length)
        Assert.Equal(1, cols.[0].Length)
        floatEqual 5.0 cols.[0].[0] 1e-10
        floatEqual 10.0 cols.[1].[0] 1e-10
        floatEqual 15.0 cols.[2].[0] 1e-10

    // ====== getRow quotation tests ======

    [<Fact>]
    let ``getRow Q: extracts correct row`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]])
        let row1 = eval <@ Matrix.getRow 1 m @>

        Assert.Equal(3, row1.Length)
        Assert.Equal(4, row1.[0])
        Assert.Equal(5, row1.[1])
        Assert.Equal(6, row1.[2])

    [<Fact>]
    let ``getRow Q: first row`` () =
        let m = Matrix.ofArray2D (array2D [[10.0; 20.0; 30.0]; [40.0; 50.0; 60.0]])
        let row0 = eval <@ Matrix.getRow 0 m @>

        Assert.Equal(3, row0.Length)
        floatEqual 10.0 row0.[0] 1e-10
        floatEqual 30.0 row0.[2] 1e-10

    [<Fact>]
    let ``getRow Q: throws on negative index`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.getRow -1 m @> |> ignore)

    [<Fact>]
    let ``getRow Q: throws on index out of bounds`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.getRow 2 m @> |> ignore)

    // ====== getRows quotation tests ======

    [<Fact>]
    let ``getRows Q: extracts all rows`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]; [5; 6]])
        let rows = eval <@ Matrix.getRows m @>

        Assert.Equal(3, rows.Length)
        Assert.Equal(2, rows.[0].Length)
        Assert.Equal(1, rows.[0].[0])
        Assert.Equal(2, rows.[0].[1])
        Assert.Equal(5, rows.[2].[0])
        Assert.Equal(6, rows.[2].[1])

    // ====== diagonal quotation tests ======

    [<Fact>]
    let ``diagonal Q: creates diagonal matrix from vector`` () =
        let v = [| 1; 2; 3 |]
        let m = eval <@ Matrix.diagonal v @>

        Assert.Equal(3, m.NumRows)
        Assert.Equal(3, m.NumCols)
        Assert.Equal(1, m.[0, 0])
        Assert.Equal(2, m.[1, 1])
        Assert.Equal(3, m.[2, 2])
        Assert.Equal(0, m.[0, 1])
        Assert.Equal(0, m.[1, 0])

    [<Fact>]
    let ``diagonal Q: single element`` () =
        let v = [| 42.0 |]
        let m = eval <@ Matrix.diagonal v @>

        Assert.Equal(1, m.NumRows)
        Assert.Equal(1, m.NumCols)
        floatEqual 42.0 m.[0, 0] 1e-10

    [<Fact>]
    let ``diagonal Q: larger diagonal matrix`` () =
        let v = [| 10; 20; 30; 40; 50 |]
        let m = eval <@ Matrix.diagonal v @>

        Assert.Equal(5, m.NumRows)
        Assert.Equal(5, m.NumCols)
        Assert.Equal(10, m.[0, 0])
        Assert.Equal(50, m.[4, 4])
        Assert.Equal(0, m.[0, 4])
        Assert.Equal(0, m.[4, 0])

    // ====== getDiagonal quotation tests ======

    [<Fact>]
    let ``getDiagonal Q: extracts diagonal from square matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]])
        let diag = eval <@ Matrix.getDiagonal m @>

        Assert.Equal(3, diag.Length)
        Assert.Equal(1, diag.[0])
        Assert.Equal(5, diag.[1])
        Assert.Equal(9, diag.[2])

    [<Fact>]
    let ``getDiagonal Q: extracts from rectangular tall matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]; [5.0; 6.0]])
        let diag = eval <@ Matrix.getDiagonal m @>

        Assert.Equal(2, diag.Length)
        floatEqual 1.0 diag.[0] 1e-10
        floatEqual 4.0 diag.[1] 1e-10

    [<Fact>]
    let ``getDiagonal Q: extracts from rectangular wide matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3; 4]; [5; 6; 7; 8]])
        let diag = eval <@ Matrix.getDiagonal m @>

        Assert.Equal(2, diag.Length)
        Assert.Equal(1, diag.[0])
        Assert.Equal(6, diag.[1])

    [<Fact>]
    let ``getDiagonal Q: single element matrix`` () =
        let m = Matrix.ofArray2D (array2D [[99]])
        let diag = eval <@ Matrix.getDiagonal m @>

        Assert.Equal(1, diag.Length)
        Assert.Equal(99, diag.[0])

    // ====== identity quotation tests ======

    [<Fact>]
    let ``identity Q: creates identity matrix`` () =
        let I = eval <@ Matrix.identity<int> 3 @>

        Assert.Equal(3, I.NumRows)
        Assert.Equal(3, I.NumCols)
        Assert.Equal(1, I.[0, 0])
        Assert.Equal(1, I.[1, 1])
        Assert.Equal(1, I.[2, 2])
        Assert.Equal(0, I.[0, 1])
        Assert.Equal(0, I.[1, 0])

    [<Fact>]
    let ``identity Q: single element identity`` () =
        let I = eval <@ Matrix.identity<double> 1 @>

        Assert.Equal(1, I.NumRows)
        Assert.Equal(1, I.NumCols)
        floatEqual 1.0 I.[0, 0] 1e-10

    [<Fact>]
    let ``identity Q: larger identity matrix`` () =
        let I = eval <@ Matrix.identity<float> 5 @>

        Assert.Equal(5, I.NumRows)
        Assert.Equal(5, I.NumCols)
        for i in 0..4 do
            for j in 0..4 do
                if i = j then
                    floatEqual 1.0 I.[i, j] 1e-10
                else
                    floatEqual 0.0 I.[i, j] 1e-10

    // ====== zeroCreate quotation tests ======

    [<Fact>]
    let ``zeroCreate Q: creates zero matrix`` () =
        let z = eval <@ Matrix.zeroCreate<int> 2 3 @>

        Assert.Equal(2, z.NumRows)
        Assert.Equal(3, z.NumCols)
        Assert.Equal(0, z.[0, 0])
        Assert.Equal(0, z.[1, 2])

    [<Fact>]
    let ``zeroCreate Q: creates square zero matrix`` () =
        let z = eval <@ Matrix.zeroCreate<double> 4 4 @>

        Assert.Equal(4, z.NumRows)
        Assert.Equal(4, z.NumCols)
        for i in 0..3 do
            for j in 0..3 do
                floatEqual 0.0 z.[i, j] 1e-10

    // ====== ones quotation tests ======

    [<Fact>]
    let ``ones Q: creates matrix filled with ones`` () =
        let o = eval <@ Matrix.ones<int> 2 3 @>

        Assert.Equal(2, o.NumRows)
        Assert.Equal(3, o.NumCols)
        for i in 0..1 do
            for j in 0..2 do
                Assert.Equal(1, o.[i, j])

    [<Fact>]
    let ``ones Q: creates square ones matrix`` () =
        let o = eval <@ Matrix.ones<double> 3 3 @>

        Assert.Equal(3, o.NumRows)
        Assert.Equal(3, o.NumCols)
        for i in 0..2 do
            for j in 0..2 do
                floatEqual 1.0 o.[i, j] 1e-10

    [<Fact>]
    let ``ones Q: single element`` () =
        let o = eval <@ Matrix.ones<float> 1 1 @>

        Assert.Equal(1, o.NumRows)
        Assert.Equal(1, o.NumCols)
        floatEqual 1.0 o.[0, 0] 1e-10

    [<Fact>]
    let ``ones Q: larger matrix`` () =
        let o = eval <@ Matrix.ones<int> 5 7 @>

        Assert.Equal(5, o.NumRows)
        Assert.Equal(7, o.NumCols)
        for i in 0..4 do
            for j in 0..6 do
                Assert.Equal(1, o.[i, j])

    // ====== add quotation tests ======

    [<Fact>]
    let ``add Q: adds two matrices element-wise`` () =
        let a = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let b = Matrix.ofArray2D (array2D [[5; 6]; [7; 8]])
        let c = eval <@ Matrix.add a b @>

        Assert.Equal(2, c.NumRows)
        Assert.Equal(2, c.NumCols)
        Assert.Equal(6, c.[0, 0])
        Assert.Equal(8, c.[0, 1])
        Assert.Equal(10, c.[1, 0])
        Assert.Equal(12, c.[1, 1])

    [<Fact>]
    let ``add Q: throws on dimension mismatch`` () =
        let a = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let b = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.add a b @> |> ignore)

    [<Fact>]
    let ``add Q: works with floats`` () =
        let a = Matrix.ofArray2D (array2D [[1.5; 2.5]; [3.5; 4.5]])
        let b = Matrix.ofArray2D (array2D [[0.5; 0.5]; [0.5; 0.5]])
        let c = eval <@ Matrix.add a b @>

        floatEqual 2.0 c.[0, 0] 1e-10
        floatEqual 3.0 c.[0, 1] 1e-10
        floatEqual 4.0 c.[1, 0] 1e-10
        floatEqual 5.0 c.[1, 1] 1e-10

    // ====== subtract quotation tests ======

    [<Fact>]
    let ``subtract Q: subtracts matrices element-wise`` () =
        let a = Matrix.ofArray2D (array2D [[10; 20]; [30; 40]])
        let b = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let c = eval <@ Matrix.subtract a b @>

        Assert.Equal(9, c.[0, 0])
        Assert.Equal(18, c.[0, 1])
        Assert.Equal(27, c.[1, 0])
        Assert.Equal(36, c.[1, 1])

    [<Fact>]
    let ``subtract Q: throws on dimension mismatch`` () =
        let a = Matrix.ofArray2D (array2D [[1; 2]])
        let b = Matrix.ofArray2D (array2D [[1]; [2]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.subtract a b @> |> ignore)

    [<Fact>]
    let ``subtract Q: works with floats`` () =
        let a = Matrix.ofArray2D (array2D [[5.0; 10.0]; [15.0; 20.0]])
        let b = Matrix.ofArray2D (array2D [[1.5; 2.5]; [3.5; 4.5]])
        let c = eval <@ Matrix.subtract a b @>

        floatEqual 3.5 c.[0, 0] 1e-10
        floatEqual 7.5 c.[0, 1] 1e-10
        floatEqual 11.5 c.[1, 0] 1e-10
        floatEqual 15.5 c.[1, 1] 1e-10

    // ====== multiply (Hadamard) quotation tests ======

    [<Fact>]
    let ``multiply Q: element-wise multiplication`` () =
        let a = Matrix.ofArray2D (array2D [[2; 3]; [4; 5]])
        let b = Matrix.ofArray2D (array2D [[10; 20]; [30; 40]])
        let c = eval <@ Matrix.multiply a b @>

        Assert.Equal(20, c.[0, 0])
        Assert.Equal(60, c.[0, 1])
        Assert.Equal(120, c.[1, 0])
        Assert.Equal(200, c.[1, 1])

    [<Fact>]
    let ``multiply Q: throws on dimension mismatch`` () =
        let a = Matrix.ofArray2D (array2D [[1; 2; 3]])
        let b = Matrix.ofArray2D (array2D [[1; 2]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.multiply a b @> |> ignore)

    [<Fact>]
    let ``multiply Q: works with floats`` () =
        let a = Matrix.ofArray2D (array2D [[2.0; 3.0]; [4.0; 5.0]])
        let b = Matrix.ofArray2D (array2D [[0.5; 0.5]; [0.5; 0.5]])
        let c = eval <@ Matrix.multiply a b @>

        floatEqual 1.0 c.[0, 0] 1e-10
        floatEqual 1.5 c.[0, 1] 1e-10
        floatEqual 2.0 c.[1, 0] 1e-10
        floatEqual 2.5 c.[1, 1] 1e-10

    // ====== divide quotation tests ======

    [<Fact>]
    let ``divide Q: element-wise division`` () =
        let a = Matrix.ofArray2D (array2D [[10.0; 20.0]; [30.0; 40.0]])
        let b = Matrix.ofArray2D (array2D [[2.0; 4.0]; [5.0; 8.0]])
        let c = eval <@ Matrix.divide a b @>

        floatEqual 5.0 c.[0, 0] 1e-10
        floatEqual 5.0 c.[0, 1] 1e-10
        floatEqual 6.0 c.[1, 0] 1e-10
        floatEqual 5.0 c.[1, 1] 1e-10

    [<Fact>]
    let ``divide Q: throws on dimension mismatch`` () =
        let a = Matrix.ofArray2D (array2D [[1.0; 2.0]])
        let b = Matrix.ofArray2D (array2D [[1.0]; [2.0]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.divide a b @> |> ignore)

    // ====== addScalar quotation tests ======

    [<Fact>]
    let ``addScalar Q: adds scalar to each element`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = eval <@ Matrix.addScalar m 10 @>

        Assert.Equal(11, result.[0, 0])
        Assert.Equal(12, result.[0, 1])
        Assert.Equal(13, result.[1, 0])
        Assert.Equal(14, result.[1, 1])

    [<Fact>]
    let ``addScalar Q: works with floats`` () =
        let m = Matrix.ofArray2D (array2D [[1.5; 2.5]; [3.5; 4.5]])
        let result = eval <@ Matrix.addScalar m 0.5 @>

        floatEqual 2.0 result.[0, 0] 1e-10
        floatEqual 3.0 result.[0, 1] 1e-10
        floatEqual 4.0 result.[1, 0] 1e-10
        floatEqual 5.0 result.[1, 1] 1e-10

    [<Fact>]
    let ``addScalar Q: adding zero returns same values`` () =
        let m = Matrix.ofArray2D (array2D [[5; 10]; [15; 20]])
        let result = eval <@ Matrix.addScalar m 0 @>

        Assert.Equal(5, result.[0, 0])
        Assert.Equal(10, result.[0, 1])
        Assert.Equal(15, result.[1, 0])
        Assert.Equal(20, result.[1, 1])

    // ====== subtractScalar quotation tests ======

    [<Fact>]
    let ``subtractScalar Q: subtracts scalar from each element`` () =
        let m = Matrix.ofArray2D (array2D [[10; 20]; [30; 40]])
        let result = eval <@ Matrix.subtractScalar m 5 @>

        Assert.Equal(5, result.[0, 0])
        Assert.Equal(15, result.[0, 1])
        Assert.Equal(25, result.[1, 0])
        Assert.Equal(35, result.[1, 1])

    [<Fact>]
    let ``subtractScalar Q: works with floats`` () =
        let m = Matrix.ofArray2D (array2D [[5.0; 10.0]; [15.0; 20.0]])
        let result = eval <@ Matrix.subtractScalar m 2.5 @>

        floatEqual 2.5 result.[0, 0] 1e-10
        floatEqual 7.5 result.[0, 1] 1e-10
        floatEqual 12.5 result.[1, 0] 1e-10
        floatEqual 17.5 result.[1, 1] 1e-10

    // ====== multiplyScalar quotation tests ======

    [<Fact>]
    let ``multiplyScalar Q: multiplies each element by scalar`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let result = eval <@ Matrix.multiplyScalar m 10 @>

        Assert.Equal(10, result.[0, 0])
        Assert.Equal(20, result.[0, 1])
        Assert.Equal(30, result.[1, 0])
        Assert.Equal(40, result.[1, 1])

    [<Fact>]
    let ``multiplyScalar Q: works with floats`` () =
        let m = Matrix.ofArray2D (array2D [[2.0; 3.0]; [4.0; 5.0]])
        let result = eval <@ Matrix.multiplyScalar m 0.5 @>

        floatEqual 1.0 result.[0, 0] 1e-10
        floatEqual 1.5 result.[0, 1] 1e-10
        floatEqual 2.0 result.[1, 0] 1e-10
        floatEqual 2.5 result.[1, 1] 1e-10

    [<Fact>]
    let ``multiplyScalar Q: multiplying by zero gives zero matrix`` () =
        let m = Matrix.ofArray2D (array2D [[5.0; 10.0]; [15.0; 20.0]])
        let result = eval <@ Matrix.multiplyScalar m 0.0 @>

        floatEqual 0.0 result.[0, 0] 1e-10
        floatEqual 0.0 result.[0, 1] 1e-10
        floatEqual 0.0 result.[1, 0] 1e-10
        floatEqual 0.0 result.[1, 1] 1e-10

    // ====== divideScalar quotation tests ======

    [<Fact>]
    let ``divideScalar Q: divides each element by scalar`` () =
        let m = Matrix.ofArray2D (array2D [[10.0; 20.0]; [30.0; 40.0]])
        let result = eval <@ Matrix.divideScalar m 10.0 @>

        floatEqual 1.0 result.[0, 0] 1e-10
        floatEqual 2.0 result.[0, 1] 1e-10
        floatEqual 3.0 result.[1, 0] 1e-10
        floatEqual 4.0 result.[1, 1] 1e-10

    [<Fact>]
    let ``divideScalar Q: works with fractional divisor`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let result = eval <@ Matrix.divideScalar m 0.5 @>

        floatEqual 2.0 result.[0, 0] 1e-10
        floatEqual 4.0 result.[0, 1] 1e-10
        floatEqual 6.0 result.[1, 0] 1e-10
        floatEqual 8.0 result.[1, 1] 1e-10

    // ====== muliplyVector quotation tests ======

    [<Fact>]
    let ``muliplyVector Q: matrix-vector multiplication`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]])
        let v = [| 1.0; 2.0; 3.0 |]
        let result = eval <@ Matrix.muliplyVector m v @>

        Assert.Equal(2, result.Length)
        floatEqual 14.0 result.[0] 1e-10  // 1*1 + 2*2 + 3*3 = 14
        floatEqual 32.0 result.[1] 1e-10  // 4*1 + 5*2 + 6*3 = 32

    [<Fact>]
    let ``muliplyVector Q: throws on dimension mismatch`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.muliplyVector m v @> |> ignore)

    [<Fact>]
    let ``muliplyVector Q: identity matrix`` () =
        let I = Matrix.identity<double> 3
        let v = [| 5.0; 10.0; 15.0 |]
        let result = eval <@ Matrix.muliplyVector I v @>

        Assert.Equal(3, result.Length)
        floatEqual 5.0 result.[0] 1e-10
        floatEqual 10.0 result.[1] 1e-10
        floatEqual 15.0 result.[2] 1e-10

    [<Fact>]
    let ``muliplyVector Q: zero vector`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let v = [| 0.0; 0.0 |]
        let result = eval <@ Matrix.muliplyVector m v @>

        Assert.Equal(2, result.Length)
        floatEqual 0.0 result.[0] 1e-10
        floatEqual 0.0 result.[1] 1e-10

    // ====== multiplyRowVector quotation tests ======

    [<Fact>]
    let ``multiplyRowVector Q: row-vector-matrix multiplication`` () =
        let v = [| 1.0; 2.0 |]
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]])
        let result = eval <@ Matrix.multiplyRowVector v m @>

        Assert.Equal(3, result.Length)
        floatEqual 9.0 result.[0] 1e-10   // 1*1 + 2*4 = 9
        floatEqual 12.0 result.[1] 1e-10  // 1*2 + 2*5 = 12
        floatEqual 15.0 result.[2] 1e-10  // 1*3 + 2*6 = 15

    [<Fact>]
    let ``multiplyRowVector Q: throws on dimension mismatch`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.multiplyRowVector v m @> |> ignore)

    [<Fact>]
    let ``multiplyRowVector Q: identity matrix`` () =
        let v = [| 5.0; 10.0; 15.0 |]
        let I = Matrix.identity<double> 3
        let result = eval <@ Matrix.multiplyRowVector v I @>

        Assert.Equal(3, result.Length)
        floatEqual 5.0 result.[0] 1e-10
        floatEqual 10.0 result.[1] 1e-10
        floatEqual 15.0 result.[2] 1e-10

    // ====== addRowVector quotation tests ======

    [<Fact>]
    let ``addRowVector Q: broadcasts row vector to each row`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]])
        let v = [| 10.0; 20.0; 30.0 |]
        let result = eval <@ Matrix.addRowVector m v @>

        Assert.Equal(2, result.NumRows)
        Assert.Equal(3, result.NumCols)
        floatEqual 11.0 result.[0, 0] 1e-10
        floatEqual 22.0 result.[0, 1] 1e-10
        floatEqual 33.0 result.[0, 2] 1e-10
        floatEqual 14.0 result.[1, 0] 1e-10
        floatEqual 25.0 result.[1, 1] 1e-10
        floatEqual 36.0 result.[1, 2] 1e-10

    [<Fact>]
    let ``addRowVector Q: throws on dimension mismatch`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.addRowVector m v @> |> ignore)

    [<Fact>]
    let ``addRowVector Q: zero vector`` () =
        let m = Matrix.ofArray2D (array2D [[5.0; 10.0]; [15.0; 20.0]])
        let v = [| 0.0; 0.0 |]
        let result = eval <@ Matrix.addRowVector m v @>

        floatEqual 5.0 result.[0, 0] 1e-10
        floatEqual 10.0 result.[0, 1] 1e-10
        floatEqual 15.0 result.[1, 0] 1e-10
        floatEqual 20.0 result.[1, 1] 1e-10

    // ====== addColVector quotation tests ======

    [<Fact>]
    let ``addColVector Q: broadcasts column vector to each column`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]])
        let v = [| 10.0; 20.0 |]
        let result = eval <@ Matrix.addColVector m v @>

        Assert.Equal(2, result.NumRows)
        Assert.Equal(3, result.NumCols)
        floatEqual 11.0 result.[0, 0] 1e-10
        floatEqual 12.0 result.[0, 1] 1e-10
        floatEqual 13.0 result.[0, 2] 1e-10
        floatEqual 24.0 result.[1, 0] 1e-10
        floatEqual 25.0 result.[1, 1] 1e-10
        floatEqual 26.0 result.[1, 2] 1e-10

    [<Fact>]
    let ``addColVector Q: throws on dimension mismatch`` () =
        let m = Matrix.ofArray2D (array2D [[1.0; 2.0]; [3.0; 4.0]])
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.addColVector m v @> |> ignore)

    [<Fact>]
    let ``addColVector Q: zero vector`` () =
        let m = Matrix.ofArray2D (array2D [[5.0; 10.0]; [15.0; 20.0]; [25.0; 30.0]])
        let v = [| 0.0; 0.0; 0.0 |]
        let result = eval <@ Matrix.addColVector m v @>

        floatEqual 5.0 result.[0, 0] 1e-10
        floatEqual 10.0 result.[0, 1] 1e-10
        floatEqual 15.0 result.[1, 0] 1e-10
        floatEqual 30.0 result.[2, 1] 1e-10

    // ====== checkSameShape quotation tests ======

    [<Fact>]
    let ``checkSameShape Q: passes for same dimensions`` () =
        let a = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let b = Matrix.ofArray2D (array2D [[5; 6]; [7; 8]])
        // Should not throw
        eval <@ Matrix.checkSameShape a b @>

    [<Fact>]
    let ``checkSameShape Q: throws on different dimensions`` () =
        let a = Matrix.ofArray2D (array2D [[1; 2]; [3; 4]])
        let b = Matrix.ofArray2D (array2D [[5; 6; 7]; [8; 9; 10]])
        Assert.Throws<ArgumentException>(fun () -> eval <@ Matrix.checkSameShape a b @> |> ignore)

    // ====== transpose quotation tests ======

    [<Fact>]
    let ``transpose Q: transposes matrix`` () =
        let m = Matrix.ofArray2D (array2D [[1; 2; 3]; [4; 5; 6]])
        let t = eval <@ Matrix.transpose m @>

        Assert.Equal(3, t.NumRows)
        Assert.Equal(2, t.NumCols)
        Assert.Equal(1, t.[0, 0])
        Assert.Equal(4, t.[0, 1])
        Assert.Equal(2, t.[1, 0])
        Assert.Equal(5, t.[1, 1])
        Assert.Equal(3, t.[2, 0])
        Assert.Equal(6, t.[2, 1])

    // Note: outerProduct is not tested here due to known type error bug
    // See MatrixFloatTests.fs for skipped tests with explanation
