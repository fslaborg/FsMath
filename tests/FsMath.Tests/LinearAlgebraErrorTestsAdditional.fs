namespace FsMath.Tests.LinearAlgebraErrorTestsAdditional

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle

/// Additional comprehensive error tests for LinearAlgebra module
/// Targeting specific uncovered error paths with multiple test variations

module BackSubstituteZeroDiagonalTests =

    [<Fact>]
    let ``backSubstitute throws on zero diagonal at position 0`` () =
        // Line 110: Test zero diagonal at first position
        let r = Matrix(1, 1, [| 0.0 |])
        let y = [| 5.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.backSubstitute r y |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)

    [<Fact>]
    let ``backSubstitute throws on zero diagonal in middle`` () =
        // Line 110: Test zero diagonal in the middle of matrix
        let r = Matrix(4, 4, [|
            5.0; 1.0; 2.0; 3.0;
            0.0; 0.0; 1.0; 2.0;  // Zero at r[1,1]
            0.0; 0.0; 4.0; 1.0;
            0.0; 0.0; 0.0; 3.0 |])
        let y = [| 10.0; 5.0; 8.0; 9.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.backSubstitute r y |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)

    [<Fact>]
    let ``backSubstitute throws on zero diagonal at last position`` () =
        // Line 110: Test zero diagonal at last position
        let r = Matrix(3, 3, [|
            2.0; 3.0; 4.0;
            0.0; 5.0; 1.0;
            0.0; 0.0; 0.0 |])  // Zero at r[2,2]
        let y = [| 10.0; 15.0; 18.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.backSubstitute r y |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)


module SolveTriangularSystemZeroDiagonalTests =

    [<Fact>]
    let ``solveTriangularLinearSystem throws on zero diagonal - forward sub at position 0`` () =
        // Line 281: Zero diagonal at start in forward substitution
        let L = Matrix(3, 3, [|
            0.0; 0.0; 0.0;       // Zero at L[0,0]
            1.0; 2.0; 0.0;
            2.0; 3.0; 4.0 |])
        let v = [| 1.0; 5.0; 10.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveTriangularLinearSystem L v true |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)
        Assert.Contains("K[0,0]", ex.Message)

    [<Fact>]
    let ``solveTriangularLinearSystem throws on zero diagonal - forward sub in middle`` () =
        // Line 281: Zero diagonal in middle position
        let L = Matrix(4, 4, [|
            1.0; 0.0; 0.0; 0.0;
            2.0; 3.0; 0.0; 0.0;
            1.0; 4.0; 0.0; 0.0;  // Zero at L[2,2]
            3.0; 2.0; 1.0; 5.0 |])
        let v = [| 2.0; 8.0; 10.0; 15.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveTriangularLinearSystem L v true |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)
        Assert.Contains("K[2,2]", ex.Message)

    [<Fact>]
    let ``solveTriangularLinearSystem throws on zero diagonal - backward sub at position 0`` () =
        // Line 293: Zero diagonal at start in backward substitution
        let U = Matrix(3, 3, [|
            0.0; 1.0; 2.0;       // Zero at U[0,0]
            0.0; 3.0; 4.0;
            0.0; 0.0; 5.0 |])
        let v = [| 10.0; 12.0; 15.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveTriangularLinearSystem U v false |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)
        Assert.Contains("K[0,0]", ex.Message)

    [<Fact>]
    let ``solveTriangularLinearSystem throws on zero diagonal - backward sub in middle`` () =
        // Line 293: Zero diagonal in middle position
        let U = Matrix(4, 4, [|
            5.0; 1.0; 2.0; 3.0;
            0.0; 0.0; 4.0; 1.0;  // Zero at U[1,1]
            0.0; 0.0; 3.0; 2.0;
            0.0; 0.0; 0.0; 4.0 |])
        let v = [| 20.0; 15.0; 12.0; 8.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveTriangularLinearSystem U v false |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)
        Assert.Contains("K[1,1]", ex.Message)

    [<Fact>]
    let ``solveTriangularLinearSystem throws on zero diagonal - backward sub at last`` () =
        // Line 293: Zero diagonal at last position
        let U = Matrix(2, 2, [|
            5.0; 2.0;
            0.0; 0.0 |])         // Zero at U[1,1]
        let v = [| 10.0; 5.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveTriangularLinearSystem U v false |> ignore)
        Assert.Contains("Diagonal element is zero", ex.Message)
        Assert.Contains("K[1,1]", ex.Message)


module CholeskyNonSquareTests =

    [<Fact>]
    let ``cholesky throws on 1x2 matrix`` () =
        // Line 466: Non-square matrix (more columns than rows)
        let A = Matrix(1, 2, [| 1.0; 2.0 |])
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.cholesky A |> ignore)
        Assert.Contains("matrix must be square", ex.Message)

    [<Fact>]
    let ``cholesky throws on 2x1 matrix`` () =
        // Line 466: Non-square matrix (more rows than columns)
        let A = Matrix(2, 1, [| 1.0; 2.0 |])
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.cholesky A |> ignore)
        Assert.Contains("matrix must be square", ex.Message)

    [<Fact>]
    let ``cholesky throws on 5x3 matrix`` () =
        // Line 466: Larger non-square matrix
        let A = Matrix(5, 3, Array.init 15 (fun i -> float (i + 1)))
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.cholesky A |> ignore)
        Assert.Contains("matrix must be square", ex.Message)

    [<Fact>]
    let ``cholesky throws on 3x7 matrix`` () =
        // Line 466: Wide non-square matrix
        let A = Matrix(3, 7, Array.init 21 (fun i -> float (i + 1)))
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.cholesky A |> ignore)
        Assert.Contains("matrix must be square", ex.Message)


module SolveLinearSystemsRowMismatchTests =

    [<Fact>]
    let ``solveLinearSystems throws when B has 1 row and A has 2 rows`` () =
        // Line 636: B rows don't match A rows
        let A = Matrix.identity 2
        let B = Matrix(1, 1, [| 1.0 |])
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystems A B |> ignore)
        Assert.Contains("same number of rows", ex.Message)

    [<Fact>]
    let ``solveLinearSystems throws when B has 5 rows and A has 3 rows`` () =
        // Line 636: B has more rows than A
        let A = Matrix(3, 3, Array.init 9 (fun i -> float (i + 1)))
        let B = Matrix(5, 2, Array.init 10 (fun i -> float (i + 1)))
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystems A B |> ignore)
        Assert.Contains("same number of rows", ex.Message)

    [<Fact>]
    let ``solveLinearSystems throws when B has 2 rows and A has 4 rows`` () =
        // Line 636: B has fewer rows than A
        let A = Matrix(4, 4, Array.init 16 (fun i -> float (i + 1)))
        let B = Matrix(2, 3, Array.init 6 (fun i -> float (i + 1)))
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystems A B |> ignore)
        Assert.Contains("same number of rows", ex.Message)


module SolveLinearSystemVectorMismatchTests =

    [<Fact>]
    let ``solveLinearSystem throws when b has 1 element and A has 2 rows`` () =
        // Line 661: Vector length doesn't match matrix rows
        let A = Matrix.identity 2
        let b = [| 1.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystem A b |> ignore)
        Assert.Contains("length = A.NumRows", ex.Message)

    [<Fact>]
    let ``solveLinearSystem throws when b has 5 elements and A has 3 rows`` () =
        // Line 661: Vector longer than matrix rows
        let A = Matrix(3, 3, Array.init 9 (fun i -> float (i + 1)))
        let b = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystem A b |> ignore)
        Assert.Contains("length = A.NumRows", ex.Message)

    [<Fact>]
    let ``solveLinearSystem throws when b has 2 elements and A has 5 rows`` () =
        // Line 661: Vector shorter than matrix rows
        let A = Matrix(5, 5, Array.init 25 (fun i -> float (i + 1)))
        let b = [| 1.0; 2.0 |]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystem A b |> ignore)
        Assert.Contains("length = A.NumRows", ex.Message)

    [<Fact>]
    let ``solveLinearSystem throws on completely empty vector with 1x1 matrix`` () =
        // Line 661: Empty vector edge case
        let A = Matrix(1, 1, [| 5.0 |])
        let b = [||]
        let ex = Assert.Throws<ArgumentException>(fun () ->
            LinearAlgebra.solveLinearSystem A b |> ignore)
        Assert.Contains("length = A.NumRows", ex.Message)
