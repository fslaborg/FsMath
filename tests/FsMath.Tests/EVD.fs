namespace FsMath.Tests.LinearAlgebraTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle


module GeometryTests =

    [<Fact>]
    let ``hypot: basic Pythagorean triple 3-4-5`` () =
        let result = Geometry.hypot 3.0 4.0
        floatEqual result 5.0 1e-10

    [<Fact>]
    let ``hypot: another Pythagorean triple 5-12-13`` () =
        let result = Geometry.hypot 5.0 12.0
        floatEqual result 13.0 1e-10

    [<Fact>]
    let ``hypot: with negative values`` () =
        let result = Geometry.hypot -3.0 4.0
        floatEqual result 5.0 1e-10

    [<Fact>]
    let ``hypot: both negative`` () =
        let result = Geometry.hypot -6.0 -8.0
        floatEqual result 10.0 1e-10

    [<Fact>]
    let ``hypot: with zero`` () =
        let result = Geometry.hypot 0.0 7.0
        floatEqual result 7.0 1e-10

    [<Fact>]
    let ``hypot: both zero`` () =
        let result = Geometry.hypot 0.0 0.0
        floatEqual result 0.0 1e-10

    [<Fact>]
    let ``hypot: commutative property`` () =
        let result1 = Geometry.hypot 3.0 4.0
        let result2 = Geometry.hypot 4.0 3.0
        floatEqual result1 result2 1e-10

    [<Fact>]
    let ``hypot: large values avoid overflow`` () =
        // hypot should handle large values without overflow
        let large = 1e150
        let result = Geometry.hypot large large
        let expected = large * sqrt 2.0
        floatClose Accuracy.low result expected "hypot should handle large values"

    [<Fact>]
    let ``hypot: small values avoid underflow`` () =
        let small = 1e-150
        let result = Geometry.hypot small small
        let expected = small * sqrt 2.0
        floatClose Accuracy.low result expected "hypot should handle small values"


module EVDSymmetricTests =

    [<Fact>]
    let ``symmetricEvd: identity matrix has eigenvalues of 1`` () =
        let A = Array2D.init 3 3 (fun i j -> if i = j then 1.0 else 0.0)
        let (e, v, d) = EVD.symmetricEvd A

        // All eigenvalues should be 1.0
        for i = 0 to d.Length - 1 do
            floatEqual d.[i] 1.0 1e-10

        // Imaginary parts should be zero
        for i = 0 to e.Length - 1 do
            floatEqual e.[i] 0.0 1e-10

    [<Fact>]
    let ``symmetricEvd: diagonal matrix eigenvalues are diagonal elements`` () =
        let A = Array2D.create 3 3 0.0
        A.[0,0] <- 2.0
        A.[1,1] <- 5.0
        A.[2,2] <- 3.0
        let (e, v, d) = EVD.symmetricEvd A

        // Eigenvalues should be the diagonal elements (sorted)
        let eigenvalues = Array.sort d
        let expected = [| 2.0; 3.0; 5.0 |]
        for i = 0 to 2 do
            floatClose Accuracy.medium eigenvalues.[i] expected.[i] "Eigenvalue mismatch"

    [<Fact>]
    let ``symmetricEvd: known 2x2 matrix`` () =
        // Matrix [[1, 2], [2, 1]] has eigenvalues 3 and -1
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 1.0; A.[0,1] <- 2.0
        A.[1,0] <- 2.0; A.[1,1] <- 1.0
        let (e, v, d) = EVD.symmetricEvd A

        let eigenvalues = Array.sort d
        floatClose Accuracy.medium eigenvalues.[0] -1.0 "First eigenvalue should be -1"
        floatClose Accuracy.medium eigenvalues.[1] 3.0 "Second eigenvalue should be 3"

    [<Fact>]
    let ``symmetricEvd: reconstruction A*v_i = lambda_i*v_i`` () =
        let A = Array2D.create 3 3 0.0
        A.[0,0] <- 4.0; A.[0,1] <- 2.0; A.[0,2] <- 0.0
        A.[1,0] <- 2.0; A.[1,1] <- 5.0; A.[1,2] <- 3.0
        A.[2,0] <- 0.0; A.[2,1] <- 3.0; A.[2,2] <- 6.0
        let (e, v, d) = EVD.symmetricEvd A

        // For each eigenvalue/eigenvector pair, verify A*v = lambda*v
        let n = d.Length
        for i = 0 to n - 1 do
            let lambda = d.[i]
            let eigenvector = Array.init n (fun j -> v.[j, i])

            // Compute A * eigenvector
            let Av = Array.init n (fun j ->
                let mutable sum = 0.0
                for k = 0 to n - 1 do
                    sum <- sum + A.[j, k] * eigenvector.[k]
                sum)

            // Compute lambda * eigenvector
            let lambdaV = Array.map (fun x -> lambda * x) eigenvector

            // Verify A*v = lambda*v (within tolerance)
            for j = 0 to n - 1 do
                floatClose Accuracy.low Av.[j] lambdaV.[j] $"A*v != lambda*v for eigenvalue {i}"

    [<Fact>]
    let ``symmetricEvd: eigenvectors are orthonormal`` () =
        let A = Array2D.create 3 3 0.0
        A.[0,0] <- 4.0; A.[0,1] <- 2.0; A.[0,2] <- 0.0
        A.[1,0] <- 2.0; A.[1,1] <- 5.0; A.[1,2] <- 3.0
        A.[2,0] <- 0.0; A.[2,1] <- 3.0; A.[2,2] <- 6.0
        let (e, v, d) = EVD.symmetricEvd A

        let n = d.Length
        // Check that V^T * V = I (eigenvectors are orthonormal)
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                let mutable dot = 0.0
                for k = 0 to n - 1 do
                    dot <- dot + v.[k, i] * v.[k, j]

                if i = j then
                    floatClose Accuracy.medium dot 1.0 $"v_{i} should have unit length"
                else
                    floatClose Accuracy.medium dot 0.0 $"v_{i} and v_{j} should be orthogonal"

    [<Fact>]
    let ``symmetricEvd: reconstruction V*D*V^T = A`` () =
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 4.0; A.[0,1] <- 2.0
        A.[1,0] <- 2.0; A.[1,1] <- 3.0
        let (e, v, d) = EVD.symmetricEvd A

        let n = d.Length
        // Reconstruct A from V * D * V^T
        let reconstructed = Array2D.zeroCreate n n
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                let mutable sum = 0.0
                for k = 0 to n - 1 do
                    sum <- sum + v.[i, k] * d.[k] * v.[j, k]
                reconstructed.[i, j] <- sum

        // Verify reconstruction matches original
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                floatClose Accuracy.low reconstructed.[i, j] A.[i, j] "Reconstructed matrix should match original"

    [<Fact>]
    let ``symmetricEvd: eigenvalues are sorted ascending`` () =
        let A = Array2D.create 3 3 0.0
        A.[0,0] <- 5.0; A.[0,1] <- 1.0; A.[0,2] <- 0.0
        A.[1,0] <- 1.0; A.[1,1] <- 3.0; A.[1,2] <- 1.0
        A.[2,0] <- 0.0; A.[2,1] <- 1.0; A.[2,2] <- 2.0
        let (e, v, d) = EVD.symmetricEvd A

        // Eigenvalues should be sorted in ascending order
        for i = 0 to d.Length - 2 do
            Assert.True(d.[i] <= d.[i+1], $"Eigenvalues should be sorted: d[{i}]={d.[i]} > d[{i+1}]={d.[i+1]}")

    [<Fact>]
    let ``symmetricEvd: trace equals sum of eigenvalues`` () =
        let A = Array2D.create 3 3 0.0
        A.[0,0] <- 4.0; A.[0,1] <- 2.0; A.[0,2] <- 1.0
        A.[1,0] <- 2.0; A.[1,1] <- 5.0; A.[1,2] <- 3.0
        A.[2,0] <- 1.0; A.[2,1] <- 3.0; A.[2,2] <- 6.0
        let (e, v, d) = EVD.symmetricEvd A

        // Trace of A (sum of diagonal elements)
        let trace = A.[0,0] + A.[1,1] + A.[2,2]

        // Sum of eigenvalues
        let eigenSum = Array.sum d

        floatClose Accuracy.medium eigenSum trace "Sum of eigenvalues should equal trace"

    [<Fact>]
    let ``symmetricEvd: single element matrix`` () =
        let A = Array2D.create 1 1 7.0
        let (e, v, d) = EVD.symmetricEvd A

        floatEqual d.[0] 7.0 1e-10
        floatEqual v.[0, 0] 1.0 1e-10

    [<Fact>]
    let ``symmetricEvd: positive definite matrix has positive eigenvalues`` () =
        // A positive definite matrix
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 4.0; A.[0,1] <- 1.0
        A.[1,0] <- 1.0; A.[1,1] <- 3.0
        let (e, v, d) = EVD.symmetricEvd A

        // All eigenvalues should be positive
        for i = 0 to d.Length - 1 do
            Assert.True(d.[i] > 0.0, $"Eigenvalue {i} should be positive: {d.[i]}")

    [<Fact>]
    let ``symmetricEvd: matrix with zero eigenvalue`` () =
        // Singular matrix with zero eigenvalue
        let A = Array2D.create 2 2 1.0
        let (e, v, d) = EVD.symmetricEvd A

        // Should have eigenvalue 0 and eigenvalue 2
        let eigenvalues = Array.sort d
        floatClose Accuracy.medium eigenvalues.[0] 0.0 "Should have zero eigenvalue"
        floatClose Accuracy.medium eigenvalues.[1] 2.0 "Should have eigenvalue 2"

    [<Fact>]
    let ``symmetricEvd: matrix with negative eigenvalues`` () =
        // Matrix with both positive and negative eigenvalues
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 1.0; A.[0,1] <- 2.0
        A.[1,0] <- 2.0; A.[1,1] <- 1.0
        let (e, v, d) = EVD.symmetricEvd A

        let eigenvalues = Array.sort d
        Assert.True(eigenvalues.[0] < 0.0, "Should have negative eigenvalue")
        Assert.True(eigenvalues.[1] > 0.0, "Should have positive eigenvalue")

    [<Fact>]
    let ``symmetricEvd: larger 4x4 matrix`` () =
        let A = Array2D.create 4 4 0.0
        A.[0,0] <- 4.0; A.[0,1] <- 1.0
        A.[1,0] <- 1.0; A.[1,1] <- 4.0; A.[1,2] <- 1.0
        A.[2,1] <- 1.0; A.[2,2] <- 4.0; A.[2,3] <- 1.0
        A.[3,2] <- 1.0; A.[3,3] <- 4.0
        let (e, v, d) = EVD.symmetricEvd A

        // Verify eigenvalue property for first eigenpair
        let lambda = d.[0]
        let eigenvector = Array.init 4 (fun j -> v.[j, 0])

        let Av = Array.init 4 (fun j ->
            let mutable sum = 0.0
            for k = 0 to 3 do
                sum <- sum + A.[j, k] * eigenvector.[k]
            sum)

        let lambdaV = Array.map (fun x -> lambda * x) eigenvector

        for j = 0 to 3 do
            floatClose Accuracy.low Av.[j] lambdaV.[j] "A*v = lambda*v should hold"

    [<Fact>]
    let ``symmetricEvd: determinant equals product of eigenvalues`` () =
        // For a 2x2 matrix, det(A) is easy to compute
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 6.0; A.[0,1] <- 2.0
        A.[1,0] <- 2.0; A.[1,1] <- 3.0
        let (e, v, d) = EVD.symmetricEvd A

        // det(A) = 6*3 - 2*2 = 14
        let detA = 6.0 * 3.0 - 2.0 * 2.0

        // Product of eigenvalues
        let eigenProduct = Array.fold (*) 1.0 d

        floatClose Accuracy.medium eigenProduct detA "Product of eigenvalues should equal determinant"


module EVDGetterTests =

    [<Fact>]
    let ``getRealEigenvalues returns eigenvalue array`` () =
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 2.0; A.[0,1] <- 1.0
        A.[1,0] <- 1.0; A.[1,1] <- 2.0
        let result = EVD.symmetricEvd A
        let eigenvalues = EVD.getRealEigenvalues result

        Assert.Equal(2, eigenvalues.Length)
        // Eigenvalues should be 1 and 3
        let sorted = Array.sort eigenvalues
        floatClose Accuracy.medium sorted.[0] 1.0 "First eigenvalue"
        floatClose Accuracy.medium sorted.[1] 3.0 "Second eigenvalue"

    [<Fact>]
    let ``getImaginaryEigenvalues returns imaginary parts`` () =
        let A = Array2D.init 2 2 (fun i j -> if i = j then 1.0 else 0.0)
        let result = EVD.symmetricEvd A
        let imaginary = EVD.getImaginaryEigenvalues result

        // For symmetric matrices, imaginary parts should be zero
        for i = 0 to imaginary.Length - 1 do
            floatEqual imaginary.[i] 0.0 1e-10

    [<Fact>]
    let ``getEigenvalueMAtrix returns eigenvector matrix`` () =
        let A = Array2D.create 2 2 0.0
        A.[0,0] <- 3.0; A.[0,1] <- 1.0
        A.[1,0] <- 1.0; A.[1,1] <- 3.0
        let result = EVD.symmetricEvd A
        let V = EVD.getEigenvalueMAtrix result

        Assert.Equal(2, V.GetLength(0))
        Assert.Equal(2, V.GetLength(1))

        // Verify V is orthogonal (V^T * V = I)
        let vtv00 = V.[0,0] * V.[0,0] + V.[1,0] * V.[1,0]
        let vtv11 = V.[0,1] * V.[0,1] + V.[1,1] * V.[1,1]
        let vtv01 = V.[0,0] * V.[0,1] + V.[1,0] * V.[1,1]

        floatClose Accuracy.medium vtv00 1.0 "V^TV[0,0] should be 1"
        floatClose Accuracy.medium vtv11 1.0 "V^TV[1,1] should be 1"
        floatClose Accuracy.medium vtv01 0.0 "V^TV[0,1] should be 0"
