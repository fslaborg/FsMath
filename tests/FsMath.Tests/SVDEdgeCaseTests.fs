namespace FsMath.Tests

open Xunit
open FsMath.Algebra

/// Additional edge case tests for SVD to improve coverage
/// Targets specific uncovered branches in the SVD algorithm
module SVDEdgeCaseTests =

    let private tolerance = 1e-9

    /// Helper to assert two floats are approximately equal
    let private assertApproxEqual (expected: float) (actual: float) =
        let diff = abs(expected - actual)
        Assert.True(diff < tolerance, sprintf "Expected %f but got %f (diff: %e)" expected actual diff)

    /// Helper to assert two matrices are approximately equal
    let private assertMatrixApproxEqual (expected: float[,]) (actual: float[,]) =
        let m1 = expected.GetLength(0)
        let n1 = expected.GetLength(1)
        let m2 = actual.GetLength(0)
        let n2 = actual.GetLength(1)
        Assert.Equal(m1, m2)
        Assert.Equal(n1, n2)
        for i in 0 .. m1-1 do
            for j in 0 .. n1-1 do
                let diff = abs(expected.[i,j] - actual.[i,j])
                Assert.True(diff < tolerance, sprintf "Mismatch at [%d,%d]: expected %f, got %f" i j expected.[i,j] actual.[i,j])

    /// Helper for matrix multiplication
    let private matmul (A: float[,]) (B: float[,]) : float[,] =
        let m = A.GetLength(0)
        let n = A.GetLength(1)
        let p = B.GetLength(1)
        Assert.Equal(n, B.GetLength(0))
        Array2D.init m p (fun i k ->
            let mutable sum = 0.0
            for j in 0 .. n-1 do
                sum <- sum + A.[i,j] * B.[j,k]
            sum
        )

    /// Helper to convert singular values array to diagonal matrix
    let private singularValuesToMatrix (s: float[]) (m: int) (n: int) : float[,] =
        Array2D.init m n (fun i j ->
            if i = j && i < s.Length then s.[i]
            else 0.0
        )

    /// Helper to transpose a matrix
    let private transpose (A: float[,]) : float[,] =
        let m = A.GetLength(0)
        let n = A.GetLength(1)
        Array2D.init n m (fun i j -> A.[j,i])

    // ===== Tests targeting uncovered lines =====

    [<Fact>]
    let ``SVD handles matrix with negligible singular value (Case 2 path)`` () =
        // Create a matrix designed to have a near-zero singular value
        // This should trigger Case 2: s(k) is negligible
        let A = array2D [[1e-15; 0.0; 0.0]
                         [0.0;    5.0; 0.0]
                         [0.0;    0.0; 3.0]]
        let (U, s, Vt) = SVD.compute A

        // Should complete successfully
        Assert.Equal(3, s.Length)

        // Verify reconstruction still works
        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles rank-deficient matrix with zero column`` () =
        // Matrix with a zero column - should trigger s(k) = 0 paths
        let A = array2D [[1.0; 0.0; 2.0]
                         [3.0; 0.0; 4.0]
                         [5.0; 0.0; 6.0]]
        let (U, s, Vt) = SVD.compute A

        // Verify basic properties
        Assert.Equal(3, s.Length)

        // One singular value should be very small (corresponding to zero column)
        let minSV = Array.min s
        Assert.True(minSV < 1e-10, sprintf "Expected one very small singular value, got min=%e" minSV)

        // Reconstruction should work
        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix with zero diagonal entry`` () =
        // Matrix designed to have zero on diagonal after bidiagonalization
        // This should trigger the s.[k] = 0.0 path in U generation (lines 326-329)
        let A = array2D [[0.0; 1.0; 2.0]
                         [0.0; 3.0; 4.0]
                         [0.0; 5.0; 6.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // Verify U is orthogonal even with zero paths
        let UtU = matmul (transpose U) U
        let I = Array2D.init 3 3 (fun i j -> if i = j then 1.0 else 0.0)
        assertMatrixApproxEqual I UtU

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles 2x3 matrix (m < n case with p adjustment)`` () =
        // This tests the condition m < p (line 293)
        // where m=2, n=3, so p = min(n, m+1) = 3, and m < p triggers s.[p-1] = 0.0
        let A = array2D [[1.0; 2.0; 3.0]
                         [4.0; 5.0; 6.0]]
        let (U, s, Vt) = SVD.compute A

        // Basic validation
        Assert.Equal(2, s.Length)

        // All singular values should be non-negative
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] >= 0.0)

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s 2 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles very ill-conditioned matrix (negative b in shift)`` () =
        // Create a matrix that triggers negative b in shift calculation (line 470)
        // Use a matrix with very different scales to create specific eigenvalue pattern
        let A = array2D [[10.0; 1.0;  0.0]
                         [1.0;  0.1;  0.01]
                         [0.0;  0.01; 0.001]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // Should still produce valid decomposition
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] >= 0.0, sprintf "Singular value s.[%d]=%f should be non-negative" i s.[i])

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix causing negative shift parameter`` () =
        // Another matrix designed to trigger b < 0 case in shift calculation
        let A = array2D [[5.0; 4.0; 3.0]
                         [4.0; 2.0; 1.0]
                         [3.0; 1.0; 0.5]]
        let (U, s, Vt) = SVD.compute A

        // Should converge successfully
        Assert.Equal(3, s.Length)
        Assert.True(s.[0] > 0.0)

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix with multiple near-zero singular values`` () =
        // Rank-1 approximation that should trigger multiple negligible s(k) cases
        let A = array2D [[1.0; 1.0; 1.0]
                         [1e-14; 1e-14; 1e-14]
                         [1e-14; 1e-14; 1e-14]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // First singular value should be significant
        Assert.True(s.[0] > 1.0, sprintf "Expected first singular value > 1, got %f" s.[0])

        // Other singular values should be near zero
        Assert.True(s.[1] < 1e-10, sprintf "Expected small second singular value, got %e" s.[1])
        Assert.True(s.[2] < 1e-10, sprintf "Expected small third singular value, got %e" s.[2])

        // Reconstruction should still work
        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles asymmetric matrix with varied scales`` () =
        // Matrix with different scales to stress different code paths
        let A = array2D [[100.0; 0.1;    0.001]
                         [10.0;  1.0;    0.01]
                         [1.0;   0.1;    0.001]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // Singular values should span multiple orders of magnitude
        Assert.True(s.[0] > 100.0)
        Assert.True(s.[2] < 1.0)

        // Verify properties
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] >= 0.0)
            Assert.True(System.Double.IsFinite(s.[i]))

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles 3x2 tall matrix with near-singular structure`` () =
        // Tall matrix with near-linear dependence
        let A = array2D [[1.0; 2.0]
                         [2.0; 4.0 + 1e-12]
                         [3.0; 6.0 + 2e-12]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(2, s.Length)

        // Should have one large and one very small singular value
        Assert.True(s.[0] > 1.0)
        Assert.True(s.[1] < 1e-8)

        let Sigma = singularValuesToMatrix s 3 2
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix requiring many iterations`` () =
        // Matrix structure that may require more iterations to converge
        let A = array2D [[1.0;  0.9;  0.8]
                         [0.9;  1.0;  0.9]
                         [0.8;  0.9;  1.0]]
        let (U, s, Vt) = SVD.compute A

        // Should eventually converge
        Assert.Equal(3, s.Length)

        // All values should be positive (symmetric positive definite)
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] > 0.0, sprintf "s.[%d]=%f should be positive" i s.[i])

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix with alternating signs`` () =
        // Matrix with alternating positive/negative entries
        let A = array2D [[1.0;  -2.0;  3.0]
                         [-4.0;  5.0; -6.0]
                         [7.0;  -8.0;  9.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // Singular values must all be non-negative
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] >= 0.0)

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles 4x2 tall narrow matrix`` () =
        // Taller narrow matrix to test m < p condition variations
        let A = array2D [[1.0; 2.0]
                         [3.0; 4.0]
                         [5.0; 6.0]
                         [7.0; 8.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(2, s.Length)

        // Check dimensions
        Assert.Equal(4, U.GetLength(0))
        Assert.Equal(4, U.GetLength(1))
        Assert.Equal(2, Vt.GetLength(0))
        Assert.Equal(2, Vt.GetLength(1))

        let Sigma = singularValuesToMatrix s 4 2
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix with one dominant singular value`` () =
        // Matrix where one singular value dominates
        let A = array2D [[1000.0; 0.001]
                         [0.001;  0.001]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(2, s.Length)

        // First singular value should be much larger
        Assert.True(s.[0] > 999.0)
        Assert.True(s.[1] < 1.0)

        let Sigma = singularValuesToMatrix s 2 2
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles tridiagonal-like structure`` () =
        // Matrix with tridiagonal-like structure
        let A = array2D [[4.0; 1.0; 0.0]
                         [1.0; 4.0; 1.0]
                         [0.0; 1.0; 4.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // All singular values should be positive
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] > 0.0)

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles matrix with repeated rows`` () =
        // Matrix with repeated rows (rank deficient)
        let A = array2D [[1.0; 2.0; 3.0]
                         [1.0; 2.0; 3.0]
                         [4.0; 5.0; 6.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // Should have at least one near-zero singular value
        let minSV = Array.min s
        Assert.True(minSV < 1e-10)

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles upper triangular matrix`` () =
        // Upper triangular matrix
        let A = array2D [[5.0; 3.0; 1.0]
                         [0.0; 4.0; 2.0]
                         [0.0; 0.0; 3.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // All singular values should be positive
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] > 0.0)

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles lower triangular matrix`` () =
        // Lower triangular matrix
        let A = array2D [[3.0; 0.0; 0.0]
                         [2.0; 4.0; 0.0]
                         [1.0; 3.0; 5.0]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // All singular values should be positive
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] > 0.0)

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles nearly singular matrix`` () =
        // Matrix that is nearly singular (very small determinant)
        let eps = 1e-13
        let A = array2D [[1.0;     1.0;     1.0]
                         [1.0;     1.0+eps; 1.0]
                         [1.0;     1.0;     1.0+eps]]
        let (U, s, Vt) = SVD.compute A

        Assert.Equal(3, s.Length)

        // Should have one large and two very small singular values
        Assert.True(s.[0] > 1.0)
        Assert.True(s.[1] < 1e-10)
        Assert.True(s.[2] < 1e-10)

        let Sigma = singularValuesToMatrix s 3 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed
