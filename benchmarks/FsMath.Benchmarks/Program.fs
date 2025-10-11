﻿open System
open BenchmarkDotNet.Running
open FsMath.Benchmarks

[<EntryPoint>]
let Main args =
    // Register multiple benchmark classes
    let switcher = BenchmarkSwitcher [|
        typeof<VectorBenchmarks>
        typeof<MatrixBenchmarks>
    |]
    switcher.Run args |> ignore
    0
