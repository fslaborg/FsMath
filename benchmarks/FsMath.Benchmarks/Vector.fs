﻿namespace FsMath.Benchmarks


open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FsMath

[<MemoryDiagnoser>]
type VectorBenchmarks() =

    let mutable vector1 = [||]
    let mutable vector2 = [||]

    // Parameterize vector sizes
    [<Params(10, 100, 1000, 10000)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        vector1 <- Array.init this.Size (fun i -> float i)
        vector2 <- Array.init this.Size (fun i -> float (i * 2))

    [<Benchmark>]
    member _.Add() =
        let result = Vector.add vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Sub() =
        let result = Vector.subtract vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Multiply() =
        let result = Vector.multiply vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.DotProduct() =
        let result = Vector.dot vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Norm() =
        let result = Vector.norm vector1
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Sum() =
        let result = Vector.sum vector1
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Product() =
        let result = Vector.product vector1
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Min() =
        let result = Vector.min vector1
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Max() =
        let result = Vector.max vector1
        GC.KeepAlive(result) // Prevents the result from being optimized away

