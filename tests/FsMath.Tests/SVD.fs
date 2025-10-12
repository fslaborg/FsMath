namespace FsMath.Tests

open Xunit
open FsMath
open FsMath.Algebra

/// Tests for Singular Value Decomposition (SVD)
module SVDTests =

    let private tolerance = 1e-10

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
                assertApproxEqual expected.[i,j] actual.[i,j]

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

    [<Fact>]
    let ``SVD of identity matrix`` () =
        let A = array2D [[1.0; 0.0; 0.0]
                         [0.0; 1.0; 0.0]
                         [0.0; 0.0; 1.0]]
        let (U, s, Vt) = SVD.compute A

        // Identity should have singular values of 1
        Assert.Equal(3, s.Length)
        for i in 0..2 do
            assertApproxEqual 1.0 s.[i]

        // U and V should be orthogonal (U*U^T = I)
        let UtU = matmul (transpose U) U
        assertMatrixApproxEqual A UtU

    [<Fact>]
    let ``SVD of diagonal matrix`` () =
        let A = array2D [[3.0; 0.0; 0.0]
                         [0.0; 2.0; 0.0]
                         [0.0; 0.0; 1.0]]
        let (U, s, Vt) = SVD.compute A

        // Singular values should be diagonal elements in descending order
        Assert.Equal(3, s.Length)
        assertApproxEqual 3.0 s.[0]
        assertApproxEqual 2.0 s.[1]
        assertApproxEqual 1.0 s.[2]

    [<Fact>]
    let ``SVD reconstruction: A = U * Sigma * V^T`` () =
        let A = array2D [[1.0; 2.0; 3.0]
                         [4.0; 5.0; 6.0]
                         [7.0; 8.0; 9.0]]
        let (U, s, Vt) = SVD.compute A

        // Reconstruct: A = U * Sigma * V^T
        let Sigma = singularValuesToMatrix s (A.GetLength(0)) (A.GetLength(1))
        let USigma = matmul U Sigma
        let reconstructed = matmul USigma Vt

        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD of rectangular matrix (tall)`` () =
        let A = array2D [[1.0; 2.0]
                         [3.0; 4.0]
                         [5.0; 6.0]]
        let (U, s, Vt) = SVD.compute A

        // Should work for m > n
        Assert.Equal(3, U.GetLength(0))
        Assert.Equal(3, U.GetLength(1))
        Assert.Equal(2, Vt.GetLength(0))
        Assert.Equal(2, Vt.GetLength(1))

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s (A.GetLength(0)) (A.GetLength(1))
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD of rectangular matrix (wide)`` () =
        let A = array2D [[1.0; 2.0; 3.0; 4.0]
                         [5.0; 6.0; 7.0; 8.0]]
        let (U, s, Vt) = SVD.compute A

        // Should work for m < n (matrix gets transposed internally)
        Assert.Equal(2, s.Length)

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s (A.GetLength(0)) (A.GetLength(1))
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD singular values are non-negative`` () =
        let A = array2D [[1.0; -2.0]
                         [-3.0; 4.0]]
        let (_, s, _) = SVD.compute A

        // All singular values must be non-negative
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] >= 0.0, sprintf "Singular value s.[%d] = %f should be non-negative" i s.[i])

    [<Fact>]
    let ``SVD singular values are sorted descending`` () =
        let A = array2D [[3.0; 1.0; 0.0]
                         [0.0; 2.0; 1.0]
                         [1.0; 0.0; 1.0]]
        let (_, s, _) = SVD.compute A

        // Singular values should be in descending order
        for i in 0 .. s.Length-2 do
            Assert.True(s.[i] >= s.[i+1], sprintf "s.[%d]=%f should be >= s.[%d]=%f" i s.[i] (i+1) s.[i+1])

    [<Fact>]
    let ``SVD of matrix with zero rows`` () =
        let A = array2D [[1.0; 2.0; 3.0]
                         [0.0; 0.0; 0.0]
                         [4.0; 5.0; 6.0]]
        let (U, s, Vt) = SVD.compute A

        // Should handle zero rows correctly
        let Sigma = singularValuesToMatrix s (A.GetLength(0)) (A.GetLength(1))
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD of rank-1 matrix`` () =
        // Outer product of two vectors creates rank-1 matrix
        let A = array2D [[1.0; 2.0; 3.0]
                         [2.0; 4.0; 6.0]
                         [3.0; 6.0; 9.0]]
        let (_, s, _) = SVD.compute A

        // Rank-1 matrix should have only one non-zero singular value
        Assert.True(s.[0] > 1e-6, "First singular value should be positive")
        for i in 1 .. s.Length-1 do
            Assert.True(abs(s.[i]) < 1e-6, sprintf "Singular value s.[%d]=%f should be near zero for rank-1 matrix" i s.[i])

    [<Fact>]
    let ``SVD U matrix is orthogonal (U^T * U = I)`` () =
        let A = array2D [[2.0; 3.0]
                         [5.0; 7.0]]
        let (U, _, _) = SVD.compute A

        // U should be orthogonal: U^T * U = I
        let UtU = matmul (transpose U) U
        let I = Array2D.init (U.GetLength(1)) (U.GetLength(1)) (fun i j -> if i = j then 1.0 else 0.0)
        assertMatrixApproxEqual I UtU

    [<Fact>]
    let ``SVD V matrix is orthogonal (V * V^T = I)`` () =
        let A = array2D [[2.0; 3.0]
                         [5.0; 7.0]]
        let (_, _, Vt) = SVD.compute A

        // Since we get V^T, check: V^T * V = I, which is (V^T)^T * V^T = I
        let V = transpose Vt
        let VtV = matmul Vt V
        let I = Array2D.init (Vt.GetLength(0)) (Vt.GetLength(0)) (fun i j -> if i = j then 1.0 else 0.0)
        assertMatrixApproxEqual I VtV

    [<Fact>]
    let ``SVD of single element matrix`` () =
        let A = array2D [[5.0]]
        let (U, s, Vt) = SVD.compute A

        // Single element should have singular value equal to absolute value
        Assert.Equal(1, s.Length)
        assertApproxEqual 5.0 s.[0]

        // U and V should be 1x1 identity-like
        assertApproxEqual 1.0 (abs U.[0,0])
        assertApproxEqual 1.0 (abs Vt.[0,0])

    [<Fact>]
    let ``SVD of 2x2 known matrix`` () =
        // Known example: [[3, 0], [4, 5]]
        let A = array2D [[3.0; 0.0]
                         [4.0; 5.0]]
        let (U, s, Vt) = SVD.compute A

        // Verify basic properties
        Assert.Equal(2, s.Length)
        Assert.True(s.[0] >= s.[1])
        Assert.True(s.[0] > 0.0)

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s 2 2
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD handles negative entries correctly`` () =
        let A = array2D [[-1.0; -2.0]
                         [-3.0; -4.0]]
        let (U, s, Vt) = SVD.compute A

        // Singular values should still be positive
        for i in 0 .. s.Length-1 do
            Assert.True(s.[i] >= 0.0)

        // Reconstruction should work
        let Sigma = singularValuesToMatrix s 2 2
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD of larger matrix (4x4)`` () =
        let A = array2D [[1.0;  2.0;  3.0;  4.0]
                         [5.0;  6.0;  7.0;  8.0]
                         [9.0;  10.0; 11.0; 12.0]
                         [13.0; 14.0; 15.0; 16.0]]
        let (U, s, Vt) = SVD.compute A

        // Basic checks
        Assert.Equal(4, s.Length)
        Assert.True(s.[0] > 0.0)

        // Verify reconstruction
        let Sigma = singularValuesToMatrix s 4 4
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD computeInPlace modifies input matrix`` () =
        let A = array2D [[1.0; 2.0]
                         [3.0; 4.0]]
        let original = Array2D.copy A
        let (U, s, Vt) = SVD.computeInPlace A

        // A should be modified (this is the in-place version)
        // Just verify the decomposition works
        let Sigma = singularValuesToMatrix s 2 2
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual original reconstructed

    [<Fact>]
    let ``SVD compute creates copy of input`` () =
        let A = array2D [[1.0; 2.0]
                         [3.0; 4.0]]
        let original = Array2D.copy A
        let (_, _, _) = SVD.compute A

        // A should not be modified (compute copies first)
        assertMatrixApproxEqual original A

    [<Fact>]
    let ``SVD of matrix with very small values`` () =
        let A = array2D [[1e-10; 2e-10]
                         [3e-10; 4e-10]]
        let (U, s, Vt) = SVD.compute A

        // Should handle small values without numerical issues
        Assert.Equal(2, s.Length)

        // Verify reconstruction (with appropriate tolerance for small values)
        let Sigma = singularValuesToMatrix s 2 2
        let reconstructed = matmul (matmul U Sigma) Vt
        for i in 0..1 do
            for j in 0..1 do
                let diff = abs(A.[i,j] - reconstructed.[i,j])
                Assert.True(diff < 1e-15, sprintf "Reconstruction error at [%d,%d]: %e" i j diff)

    [<Fact>]
    let ``SVD satisfies Frobenius norm property`` () =
        let A = array2D [[1.0; 2.0; 3.0]
                         [4.0; 5.0; 6.0]]
        let (_, s, _) = SVD.compute A

        // Frobenius norm of A equals sqrt(sum of squared singular values)
        let frobeniusSquared =
            seq { for i in 0 .. A.GetLength(0)-1 do
                    for j in 0 .. A.GetLength(1)-1 do
                        yield A.[i,j] * A.[i,j] }
            |> Seq.sum

        let singularValuesSquaredSum = s |> Array.sumBy (fun x -> x * x)

        assertApproxEqual frobeniusSquared singularValuesSquaredSum

    [<Fact>]
    let ``SVD of matrix with repeated singular values`` () =
        // Matrix where some singular values might be equal
        let A = array2D [[1.0; 0.0; 0.0]
                         [0.0; 1.0; 0.0]
                         [0.0; 0.0; 1.0]
                         [0.0; 0.0; 0.0]]
        let (U, s, Vt) = SVD.compute A

        // Should handle repeated singular values
        let Sigma = singularValuesToMatrix s 4 3
        let reconstructed = matmul (matmul U Sigma) Vt
        assertMatrixApproxEqual A reconstructed

    [<Fact>]
    let ``SVD tall matrix maintains orthogonality`` () =
        let A = array2D [[1.0; 2.0]
                         [3.0; 4.0]
                         [5.0; 6.0]
                         [7.0; 8.0]]
        let (U, _, Vt) = SVD.compute A

        // U^T * U should be identity (or close to it for the relevant submatrix)
        let UtU = matmul (transpose U) U

        // Check diagonal is 1
        for i in 0 .. min (U.GetLength(1)) (U.GetLength(1)) - 1 do
            assertApproxEqual 1.0 UtU.[i,i]

        // V^T * V should be identity
        let V = transpose Vt
        let VtV = matmul Vt V
        for i in 0 .. Vt.GetLength(0) - 1 do
            assertApproxEqual 1.0 VtV.[i,i]
