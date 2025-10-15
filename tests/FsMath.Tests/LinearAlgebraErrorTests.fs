namespace FsMath.Tests.LinearAlgebraErrorTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle

/// Tests for error handling paths in LinearAlgebra module
/// These tests target previously uncovered exception paths

module BackSubstituteErrorTests =

    [<Fact>]
    let ``Throws ArgumentException on zero diagonal during back substitution`` () =
        // Create an upper triangular matrix with zero on diagonal at position [1,1]
        // This tests line 110 in LinearAlgebra.fs
        let r = Matrix(3, 3, [| 2.0; 3.0; 4.0;
                               0.0; 0.0; 5.0;   // Zero at r[1,1]
                               0.0; 0.0; 6.0 |])
        let y = [| 10.0; 15.0; 18.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.backSubstitute r y |> ignore) "should throw on zero diagonal"


module SolveTriangularLinearSystemErrorTests =

    [<Fact>]
    let ``Throws ArgumentException on zero diagonal in forward substitution (lower triangular)`` () =
        // Test error path at line 281 in LinearAlgebra.fs
        // Create lower triangular with zero diagonal at L[1,1]
        let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                               2.0; 0.0; 0.0;   // Zero at L[1,1]
                               3.0; 4.0; 1.0 |])
        let b = [| 2.0; 5.0; 20.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystem L b true |> ignore) "should throw on zero diagonal in forward substitution"

    [<Fact>]
    let ``Throws ArgumentException on zero diagonal in backward substitution (upper triangular)`` () =
        // Test error path at line 293 in LinearAlgebra.fs
        // Create upper triangular with zero diagonal at U[1,1]
        let U = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                               0.0; 0.0; 5.0;   // Zero at U[1,1]
                               0.0; 0.0; 6.0 |])
        let b = [| 23.0; 9.0; 6.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystem U b false |> ignore) "should throw on zero diagonal in backward substitution"

    [<Fact>]
    let ``Throws ArgumentException on zero diagonal at last position (backward)`` () =
        // Test zero diagonal in last position during backward substitution
        let U = Matrix(2, 2, [| 2.0; 3.0;
                               0.0; 0.0 |])   // Zero at U[1,1]
        let b = [| 10.0; 5.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystem U b false |> ignore) "should throw on zero diagonal at end"

    [<Fact>]
    let ``Throws ArgumentException on zero diagonal at first position (forward)`` () =
        // Test zero diagonal in first position during forward substitution
        let L = Matrix(2, 2, [| 0.0; 0.0;       // Zero at L[0,0]
                               1.0; 2.0 |])
        let b = [| 1.0; 5.0 |]
        throws<ArgumentException>(fun () -> LinearAlgebra.solveTriangularLinearSystem L b true |> ignore) "should throw on zero diagonal at start"


module CholeskyErrorTests =

    [<Fact>]
    let ``Throws ArgumentException on non-square matrix for cholesky`` () =
        // Test error path at line 466 in LinearAlgebra.fs
        let A = Matrix(3, 2, [| 4.0; 2.0;
                               2.0; 3.0;
                               1.0; 1.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.cholesky A |> ignore) "should throw on non-square matrix"

    [<Fact>]
    let ``Throws ArgumentException on 4x2 rectangular matrix`` () =
        // Additional test for non-square error path
        let A = Matrix(4, 2, [| 1.0; 2.0;
                               3.0; 4.0;
                               5.0; 6.0;
                               7.0; 8.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.cholesky A |> ignore) "should throw on 4x2 matrix"

    [<Fact>]
    let ``Throws ArgumentException on 2x4 rectangular matrix`` () =
        // Test with more columns than rows
        let A = Matrix(2, 4, [| 1.0; 2.0; 3.0; 4.0;
                               5.0; 6.0; 7.0; 8.0 |])
        throws<ArgumentException>(fun () -> LinearAlgebra.cholesky A |> ignore) "should throw on 2x4 matrix"


module SolveLinearSystemsErrorTests =

    [<Fact>]
    let ``Throws ArgumentException when B rows don't match A rows in solveLinearSystems`` () =
        // Test error path at line 636 in LinearAlgebra.fs
        let A = Matrix.identity 3
        let B = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])  // Only 2 rows, but A has 3
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystems A B |> ignore) "should throw on row mismatch"

    [<Fact>]
    let ``Throws ArgumentException when B has fewer rows than A`` () =
        // Additional test for row mismatch
        let A = Matrix(4, 4, Array.init 16 (fun i -> float (i + 1)))
        let B = Matrix(3, 2, [| 1.0; 2.0;
                               3.0; 4.0;
                               5.0; 6.0 |])  // 3 rows vs A's 4 rows
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystems A B |> ignore) "should throw when B has fewer rows"

    [<Fact>]
    let ``Throws ArgumentException when B has more rows than A`` () =
        // Test opposite direction
        let A = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        let B = Matrix(3, 2, [| 1.0; 2.0;
                               3.0; 4.0;
                               5.0; 6.0 |])  // 3 rows vs A's 2 rows
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystems A B |> ignore) "should throw when B has more rows"


module SolveLinearSystemErrorTests =

    [<Fact>]
    let ``Throws ArgumentException when vector b length doesn't match A rows in solveLinearSystem`` () =
        // Test error path at line 661 in LinearAlgebra.fs
        let A = Matrix.identity 3
        let b = [| 1.0; 2.0 |]  // Only 2 elements, but A has 3 rows
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystem A b |> ignore) "should throw on dimension mismatch"

    [<Fact>]
    let ``Throws ArgumentException when vector b is shorter than A rows`` () =
        // Additional test
        let A = Matrix(4, 4, Array.init 16 (fun i -> float (i + 1)))
        let b = [| 1.0; 2.0; 3.0 |]  // 3 elements vs A's 4 rows
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystem A b |> ignore) "should throw when b is shorter"

    [<Fact>]
    let ``Throws ArgumentException when vector b is longer than A rows`` () =
        // Test opposite direction
        let A = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        let b = [| 1.0; 2.0; 3.0 |]  // 3 elements vs A's 2 rows
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystem A b |> ignore) "should throw when b is longer"

    [<Fact>]
    let ``Throws ArgumentException with empty vector on non-empty matrix`` () =
        // Edge case: empty vector
        let A = Matrix(2, 2, [| 1.0; 2.0;
                               3.0; 4.0 |])
        let b = [||]  // Empty vector
        throws<ArgumentException>(fun () -> LinearAlgebra.solveLinearSystem A b |> ignore) "should throw on empty vector"
