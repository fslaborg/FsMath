namespace FsMath.Benchmarks

open System
open BenchmarkDotNet.Attributes
open FsMath

[<MemoryDiagnoser>]
type MatrixBenchmarks() =

    let mutable matrixA = Unchecked.defaultof<Matrix<float>>
    let mutable matrixB = Unchecked.defaultof<Matrix<float>>
    let mutable vector = [||]

    // Parameterize matrix sizes: small, medium
    [<Params(10, 50, 100)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // Initialize square matrices with some values
        matrixA <- Matrix.init<float> this.Size this.Size (fun i j -> float (i + j))
        matrixB <- Matrix.init<float> this.Size this.Size (fun i j -> float (i * 2 + j))
        vector <- Array.init this.Size (fun i -> float i)

    // Element-wise operations

    [<Benchmark>]
    member _.ElementWiseAdd() =
        let result = Matrix.add matrixA matrixB
        GC.KeepAlive(result)

    [<Benchmark>]
    member _.ElementWiseSubtract() =
        let result = Matrix.subtract matrixA matrixB
        GC.KeepAlive(result)

    [<Benchmark>]
    member _.ElementWiseMultiply() =
        let result = Matrix.multiply matrixA matrixB
        GC.KeepAlive(result)

    [<Benchmark>]
    member _.ElementWiseDivide() =
        let result = Matrix.divide matrixA matrixB
        GC.KeepAlive(result)

    // Scalar operations

    [<Benchmark>]
    member _.ScalarAdd() =
        let result = Matrix.addScalar matrixA 5.0
        GC.KeepAlive(result)

    [<Benchmark>]
    member _.ScalarMultiply() =
        let result = Matrix.multiplyScalar matrixA 2.5
        GC.KeepAlive(result)

    // Matrix multiplication

    [<Benchmark>]
    member _.MatrixMultiply() =
        let result = Matrix.matmul matrixA matrixB
        GC.KeepAlive(result)

    // Matrix-vector operations

    [<Benchmark>]
    member _.MatrixVectorMultiply() =
        let result = Matrix.muliplyVector matrixA vector
        GC.KeepAlive(result)

    [<Benchmark>]
    member _.VectorMatrixMultiply() =
        let result = Matrix.multiplyRowVector vector matrixA
        GC.KeepAlive(result)

    // Transpose

    [<Benchmark>]
    member _.Transpose() =
        let result = matrixA.Transpose()
        GC.KeepAlive(result)

    // Row/column access patterns

    [<Benchmark>]
    member this.GetRow() =
        let result = Matrix.getRow (this.Size / 2) matrixA
        GC.KeepAlive(result)

    [<Benchmark>]
    member this.GetCol() =
        let result = Matrix.getCol (this.Size / 2) matrixA
        GC.KeepAlive(result)

    // Broadcast operations

    [<Benchmark>]
    member _.AddRowVector() =
        let result = Matrix.addRowVector matrixA vector
        GC.KeepAlive(result)

    [<Benchmark>]
    member _.AddColVector() =
        let result = Matrix.addColVector matrixA vector
        GC.KeepAlive(result)
