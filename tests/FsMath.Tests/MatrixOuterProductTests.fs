module MatrixOuterProductTests

open Xunit
open FsMath

[<Fact>]
let ``Outer product produces correct dimensions`` () =
    let u = [| 1.0; 2.0; 3.0 |]
    let v = [| 4.0; 5.0 |]
    let result = Matrix.outerProduct u v
    Assert.Equal(3, result.NumRows)
    Assert.Equal(2, result.NumCols)

[<Fact>]
let ``Outer product computes correct values`` () =
    let u = [| 1.0; 2.0; 3.0 |]
    let v = [| 4.0; 5.0 |]
    let result = Matrix.outerProduct u v
    // Expected: [[1*4, 1*5], [2*4, 2*5], [3*4, 3*5]]
    //         = [[4, 5], [8, 10], [12, 15]]
    Assert.Equal(4.0, result.[0, 0])
    Assert.Equal(5.0, result.[0, 1])
    Assert.Equal(8.0, result.[1, 0])
    Assert.Equal(10.0, result.[1, 1])
    Assert.Equal(12.0, result.[2, 0])
    Assert.Equal(15.0, result.[2, 1])

[<Fact>]
let ``Outer product works with single element vectors`` () =
    let u = [| 3.0 |]
    let v = [| 7.0 |]
    let result = Matrix.outerProduct u v
    Assert.Equal(1, result.NumRows)
    Assert.Equal(1, result.NumCols)
    Assert.Equal(21.0, result.[0, 0])

[<Fact>]
let ``Outer product works with larger vectors`` () =
    let u = [| 1.0; 2.0; 3.0; 4.0 |]
    let v = [| 10.0; 20.0; 30.0 |]
    let result = Matrix.outerProduct u v
    Assert.Equal(4, result.NumRows)
    Assert.Equal(3, result.NumCols)
    // Check a few values
    Assert.Equal(10.0, result.[0, 0])  // 1 * 10
    Assert.Equal(20.0, result.[0, 1])  // 1 * 20
    Assert.Equal(30.0, result.[0, 2])  // 1 * 30
    Assert.Equal(30.0, result.[2, 0])  // 3 * 10
    Assert.Equal(80.0, result.[3, 1])  // 4 * 20
    Assert.Equal(120.0, result.[3, 2]) // 4 * 30

[<Fact>]
let ``Outer product with SIMD-friendly size`` () =
    // Size 16 ensures we use SIMD path on most systems (Vector<float>.Count is usually 4 or 8)
    let u = Array.init 10 (fun i -> float (i + 1))
    let v = Array.init 16 (fun i -> float (i + 1))
    let result = Matrix.outerProduct u v

    Assert.Equal(10, result.NumRows)
    Assert.Equal(16, result.NumCols)

    // Verify a few values
    Assert.Equal(1.0, result.[0, 0])   // 1 * 1
    Assert.Equal(16.0, result.[0, 15]) // 1 * 16
    Assert.Equal(50.0, result.[4, 9])  // 5 * 10
    Assert.Equal(160.0, result.[9, 15]) // 10 * 16
