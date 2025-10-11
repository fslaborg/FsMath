namespace FsMath.Benchmarks

open System
open BenchmarkDotNet.Attributes
open FsMath

[<MemoryDiagnoser>]
type MatrixBenchmarks() =

    let mutable vector1 = [||]
    let mutable vector2 = [||]

    // Parameterize sizes for outer product
    [<Params(10, 100, 500)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        vector1 <- Array.init this.Size (fun i -> float i)
        vector2 <- Array.init this.Size (fun i -> float (i * 2))

    [<Benchmark>]
    member _.OuterProduct() =
        let result = Matrix.outerProduct vector1 vector2
        GC.KeepAlive(result)
