namespace FsMath.Benchmarks

open BenchmarkDotNet.Attributes
open FsMath
open FsMath.Algebra

/// <summary>
/// Benchmarks for linear algebra operations (QR, LU, Cholesky, EVD).
/// These operations are fundamental to scientific computing and their performance
/// is critical for applications in statistics, machine learning, and numerical analysis.
/// </summary>
[<MemoryDiagnoser>]
type LinearAlgebraBenchmarks() =

    let mutable smallMatrix: Matrix<float> = Matrix.zeroCreate 10 10
    let mutable mediumMatrix: Matrix<float> = Matrix.zeroCreate 30 30
    let mutable largeMatrix: Matrix<float> = Matrix.zeroCreate 50 50

    let mutable smallSymmetric: Matrix<float> = Matrix.zeroCreate 10 10
    let mutable mediumSymmetric: Matrix<float> = Matrix.zeroCreate 30 30
    let mutable largeSymmetric: Matrix<float> = Matrix.zeroCreate 50 50

    let mutable smallVector: Vector<float> = Vector.zeroCreate 10
    let mutable mediumVector: Vector<float> = Vector.zeroCreate 30
    let mutable largeVector: Vector<float> = Vector.zeroCreate 50

    /// Create a random positive-definite symmetric matrix for Cholesky/EVD benchmarks
    let createSymmetricPositiveDefinite (n: int) =
        let random = System.Random(42)
        // Create a random matrix A
        let a = Matrix.init n n (fun i j -> random.NextDouble())
        // Make it symmetric positive-definite: A^T * A
        let at = Matrix.transpose a
        at * a

    /// Create a random matrix for general decompositions
    let createRandomMatrix (rows: int) (cols: int) =
        let random = System.Random(42)
        Matrix.init rows cols (fun i j -> random.NextDouble())

    /// Create a random vector
    let createRandomVector (n: int) =
        let random = System.Random(42)
        Vector.init n (fun i -> random.NextDouble())

    [<GlobalSetup>]
    member _.Setup() =
        // Initialize matrices with proper random data
        smallMatrix <- createRandomMatrix 10 10
        mediumMatrix <- createRandomMatrix 30 30
        largeMatrix <- createRandomMatrix 50 50

        smallSymmetric <- createSymmetricPositiveDefinite 10
        mediumSymmetric <- createSymmetricPositiveDefinite 30
        largeSymmetric <- createSymmetricPositiveDefinite 50

        smallVector <- createRandomVector 10
        mediumVector <- createRandomVector 30
        largeVector <- createRandomVector 50

    // ============================================
    // QR Decomposition Benchmarks
    // ============================================

    [<Benchmark>]
    member _.QR_10x10() =
        LinearAlgebra.qrDecompose smallMatrix

    [<Benchmark>]
    member _.QR_30x30() =
        LinearAlgebra.qrDecompose mediumMatrix

    [<Benchmark>]
    member _.QR_50x50() =
        LinearAlgebra.qrDecompose largeMatrix

    // ============================================
    // LU Decomposition Benchmarks
    // ============================================

    [<Benchmark>]
    member _.LU_10x10() =
        LinearAlgebra.luDecompose smallMatrix

    [<Benchmark>]
    member _.LU_30x30() =
        LinearAlgebra.luDecompose mediumMatrix

    [<Benchmark>]
    member _.LU_50x50() =
        LinearAlgebra.luDecompose largeMatrix

    // ============================================
    // Cholesky Decomposition Benchmarks
    // ============================================

    [<Benchmark>]
    member _.Cholesky_10x10() =
        LinearAlgebra.cholesky smallSymmetric

    [<Benchmark>]
    member _.Cholesky_30x30() =
        LinearAlgebra.cholesky mediumSymmetric

    [<Benchmark>]
    member _.Cholesky_50x50() =
        LinearAlgebra.cholesky largeSymmetric

    // ============================================
    // Eigenvalue Decomposition Benchmarks
    // ============================================

    [<Benchmark>]
    member _.EVD_10x10() =
        LinearAlgebra.symmetricEigenspectrum smallSymmetric

    [<Benchmark>]
    member _.EVD_30x30() =
        LinearAlgebra.symmetricEigenspectrum mediumSymmetric

    [<Benchmark>]
    member _.EVD_50x50() =
        LinearAlgebra.symmetricEigenspectrum largeSymmetric

    // ============================================
    // Linear System Solving Benchmarks
    // ============================================

    [<Benchmark>]
    member _.SolveLinearSystem_10x10() =
        LinearAlgebra.solveLinearSystem smallMatrix smallVector

    [<Benchmark>]
    member _.SolveLinearSystem_30x30() =
        LinearAlgebra.solveLinearSystem mediumMatrix mediumVector

    [<Benchmark>]
    member _.SolveLinearSystem_50x50() =
        LinearAlgebra.solveLinearSystem largeMatrix largeVector

    // ============================================
    // Matrix Inverse Benchmarks
    // ============================================

    [<Benchmark>]
    member _.Inverse_10x10() =
        LinearAlgebra.inverse smallMatrix

    [<Benchmark>]
    member _.Inverse_30x30() =
        LinearAlgebra.inverse mediumMatrix

    [<Benchmark>]
    member _.Inverse_50x50() =
        LinearAlgebra.inverse largeMatrix

    // ============================================
    // Least Squares Benchmarks
    // ============================================

    [<Benchmark>]
    member _.LeastSquares_10x10() =
        LinearAlgebra.leastSquares smallMatrix smallVector

    [<Benchmark>]
    member _.LeastSquares_30x30() =
        LinearAlgebra.leastSquares mediumMatrix mediumVector

    [<Benchmark>]
    member _.LeastSquares_50x50() =
        LinearAlgebra.leastSquares largeMatrix largeVector
