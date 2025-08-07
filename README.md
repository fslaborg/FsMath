## FsMath - Fast & Friendly Numerical Foundation for F\#

**FsMath** is a lightweight maths & statistics kernel designed for modern F# workflows.
It focuses on **zero-friction interop** with existing array-centric libraries ( *FSharp.Stats*, *Math.NET Numerics*, *libtorch* via TorchSharp, etc.) while giving you the performance head-room of **SIMD-accelerated** kernels and a clean, idiomatic F# API.

---

### Why another numeric library?

| Goal                          | What it means in practice                                                                                                                                            |
| ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Interop first**             | Core types are just aliases or thin wrappers around native F# `'T []` arrays -> you can pass data to any array-based .NET or C library without conversion or pinning.|
| **Predictable memory layout** | Matrices are stored **row-major** (C-style) to line up with libtorch / TorchSharp, modern BLAS back-ends, and GPU kernels.                                           |
| **SIMD out-of-the-box**       | Tight loops (dot, outer product, reductions ...) auto-detect hardware with SIMD support and pure-managed fallback otherwise.            |
| **F#-idiomatic API**          | Pipelining, module functions, inline operators (`.+`, `.*`, `transpose`) and SRTP “type-class” dispatch keep your notebooks & scripts concise.                       |

---

### Key features

* **Vector / RowVector / Matrix** - minimal, generic, and **structural-equality** aware.
* **High-level operators** - `.+`, `.-`, `.*`, `./`, outer product, Kronecker, hadamard, etc.
* **SIMD optimizes** - dot products, outer products, reductions, and more. 
* **Unsafe-free** - everything is pure managed IL; no native dlls to ship.
* **Perfect interop** - drop-in arrays to *FSharp.Stats*, TorchSharp, DiffSharp, and any BLAS/LAPACK P/Invoke you like.

---

### Getting started

```fsharp
open FsMath
open FSharp.Stats.Distributions

// init vector
let v1  = vector [| 1.0; 2.0; 3.0 |]

// init vector with random samples (using FSharp.Stats)
let v2 = Array.init 3 (fun _ -> Continuous.ChiSquared.Sample 30.)

// SIMD-accelerated element-wise multiply
let result = v1 .* v2              

// interop: pass 'result' straight into e.g. TorchSharp tensor
```

---

### Documentation

| Topic                                  | Where                               |
| -------------------------------------- | ----------------------------------- |
| API reference                          | `docs/` folder & generated markdown |
| SIMD design notes                      | `docs/simd.md`                      |
| Benchmarks                             | `benchmarks/`                       |
| Contributing guide                     | `CONTRIBUTING.md`                   |


---




## build

Check the [build project](./build) to take a look at the build targets.

```shell
# Windows
./build.cmd

# Linux/mac
build.sh
```

## running and writing tests

```shell
# Windows
./build.cmd runtests

# Linux/mac
build.sh runtests
```

## docs

The docs are contained in `.fsx` and `.md` files in the `docs` folder. To develop docs on a local server with hot reload, run the following in the root of the project:

```shell
# Windows
./build.cmd watchdocs

# Linux/mac
./build.sh watchdocs
```

## pack nuget package(s)
```shell
# Windows
./build.cmd pack

# Linux/mac
build.sh pack
```