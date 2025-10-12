namespace FsMath.Tests

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers
open FsMath.Tests.ExpectoStyle


module MatrixExtTests =

    [<Fact>]
    let ``Inverse: 2x2 matrix`` () =
        let A = matrix [[4.0; 7.0]; [2.0; 6.0]]
        let inv = A.Inverse()

        // Verify A * A^-1 = I
        let identity = Matrix.multiply A inv
        floatClose Accuracy.medium identity.[0, 0] 1.0 "Should be identity at [0,0]"
        floatClose Accuracy.medium identity.[1, 1] 1.0 "Should be identity at [1,1]"
        floatClose Accuracy.medium identity.[0, 1] 0.0 "Should be identity at [0,1]"
        floatClose Accuracy.medium identity.[1, 0] 0.0 "Should be identity at [1,0]"

    [<Fact>]
    let ``Inverse: 3x3 matrix`` () =
        let A = matrix [[1.0; 2.0; 3.0]; [0.0; 1.0; 4.0]; [5.0; 6.0; 0.0]]
        let inv = A.Inverse()

        // Verify A * A^-1 ≈ I
        let identity = Matrix.multiply A inv
        for i = 0 to 2 do
            for j = 0 to 2 do
                let expected = if i = j then 1.0 else 0.0
                floatClose Accuracy.low identity.[i, j] expected $"Identity check at [{i},{j}]"

    [<Fact>]
    let ``Inverse: identity matrix returns identity`` () =
        let I = Matrix.identity<float> 3
        let inv = I.Inverse()

        for i = 0 to 2 do
            for j = 0 to 2 do
                let expected = if i = j then 1.0 else 0.0
                floatClose Accuracy.high inv.[i, j] expected $"Should be identity at [{i},{j}]"

    [<Fact>]
    let ``Inverse: diagonal matrix`` () =
        let A = matrix [[2.0; 0.0; 0.0]; [0.0; 3.0; 0.0]; [0.0; 0.0; 4.0]]
        let inv = A.Inverse()

        // Inverse of diagonal matrix has reciprocals on diagonal
        floatClose Accuracy.high inv.[0, 0] 0.5 "Diagonal element 0"
        floatClose Accuracy.high inv.[1, 1] (1.0/3.0) "Diagonal element 1"
        floatClose Accuracy.high inv.[2, 2] 0.25 "Diagonal element 2"

        // Off-diagonal should be zero
        for i = 0 to 2 do
            for j = 0 to 2 do
                if i <> j then
                    floatClose Accuracy.high inv.[i, j] 0.0 $"Off-diagonal at [{i},{j}]"

    [<Fact>]
    let ``Inverse: 1x1 matrix`` () =
        let A = matrix [[5.0]]
        let inv = A.Inverse()
        floatClose Accuracy.high inv.[0, 0] 0.2 "1x1 inverse"

    [<Fact>]
    let ``Inverse: double inversion returns original`` () =
        let A = matrix [[1.0; 2.0]; [3.0; 4.0]]
        let inv = A.Inverse()
        let invInv = inv.Inverse()

        for i = 0 to 1 do
            for j = 0 to 1 do
                floatClose Accuracy.low invInv.[i, j] A.[i, j] $"Double inverse at [{i},{j}]"

    [<Fact>]
    let ``Inverse: known 2x2 matrix`` () =
        // Matrix [[1, 2], [3, 4]] has det = -2
        // Inverse should be [[-2, 1], [1.5, -0.5]]
        let A = matrix [[1.0; 2.0]; [3.0; 4.0]]
        let inv = A.Inverse()

        floatClose Accuracy.medium inv.[0, 0] -2.0 "Element [0,0]"
        floatClose Accuracy.medium inv.[0, 1] 1.0 "Element [0,1]"
        floatClose Accuracy.medium inv.[1, 0] 1.5 "Element [1,0]"
        floatClose Accuracy.medium inv.[1, 1] -0.5 "Element [1,1]"

    [<Fact>]
    let ``Inverse: 4x4 matrix`` () =
        let A = matrix [
            [1.0; 0.0; 2.0; -1.0]
            [3.0; 0.0; 0.0; 5.0]
            [2.0; 1.0; 4.0; -3.0]
            [1.0; 0.0; 5.0; 0.0]
        ]
        let inv = A.Inverse()

        // Verify A * A^-1 ≈ I
        let identity = Matrix.multiply A inv
        for i = 0 to 3 do
            for j = 0 to 3 do
                let expected = if i = j then 1.0 else 0.0
                floatClose Accuracy.low identity.[i, j] expected $"Identity check at [{i},{j}]"

    [<Fact>]
    let ``Inverse: matrix with negative elements`` () =
        let A = matrix [[-1.0; 2.0]; [3.0; -4.0]]
        let inv = A.Inverse()

        // Verify A * A^-1 = I
        let identity = Matrix.multiply A inv
        floatClose Accuracy.medium identity.[0, 0] 1.0 "Identity at [0,0]"
        floatClose Accuracy.medium identity.[1, 1] 1.0 "Identity at [1,1]"
        floatClose Accuracy.medium identity.[0, 1] 0.0 "Identity at [0,1]"
        floatClose Accuracy.medium identity.[1, 0] 0.0 "Identity at [1,0]"

    [<Fact>]
    let ``Inverse: symmetric matrix`` () =
        let A = matrix [[2.0; 1.0]; [1.0; 2.0]]
        let inv = A.Inverse()

        // Inverse of symmetric matrix should also be symmetric
        floatClose Accuracy.high inv.[0, 1] inv.[1, 0] "Inverse should be symmetric"

        // Verify correctness
        let identity = Matrix.multiply A inv
        floatClose Accuracy.medium identity.[0, 0] 1.0 "Identity at [0,0]"
        floatClose Accuracy.medium identity.[1, 1] 1.0 "Identity at [1,1]"

    [<Fact>]
    let ``Inverse: upper triangular matrix`` () =
        let A = matrix [[2.0; 3.0; 1.0]; [0.0; 1.0; 4.0]; [0.0; 0.0; 2.0]]
        let inv = A.Inverse()

        // Verify A * A^-1 ≈ I
        let identity = Matrix.multiply A inv
        for i = 0 to 2 do
            for j = 0 to 2 do
                let expected = if i = j then 1.0 else 0.0
                floatClose Accuracy.low identity.[i, j] expected $"Identity check at [{i},{j}]"

    [<Fact>]
    let ``Inverse: lower triangular matrix`` () =
        let A = matrix [[2.0; 0.0; 0.0]; [3.0; 1.0; 0.0]; [1.0; 4.0; 2.0]]
        let inv = A.Inverse()

        // Verify A * A^-1 ≈ I
        let identity = Matrix.multiply A inv
        for i = 0 to 2 do
            for j = 0 to 2 do
                let expected = if i = j then 1.0 else 0.0
                floatClose Accuracy.low identity.[i, j] expected $"Identity check at [{i},{j}]"

    [<Fact>]
    let ``Inverse: (AB)^-1 = B^-1 * A^-1`` () =
        let A = matrix [[1.0; 2.0]; [3.0; 4.0]]
        let B = matrix [[5.0; 6.0]; [7.0; 8.0]]

        let AB = Matrix.multiply A B
        let invAB = AB.Inverse()

        let invA = A.Inverse()
        let invB = B.Inverse()
        let invB_invA = Matrix.multiply invB invA

        // (AB)^-1 should equal B^-1 * A^-1
        for i = 0 to 1 do
            for j = 0 to 1 do
                floatClose Accuracy.low invAB.[i, j] invB_invA.[i, j] $"Property check at [{i},{j}]"

    [<Fact>]
    let ``Inverse: positive definite matrix`` () =
        // A positive definite matrix
        let A = matrix [[4.0; 1.0]; [1.0; 3.0]]
        let inv = A.Inverse()

        // Verify A * A^-1 = I
        let identity = Matrix.multiply A inv
        floatClose Accuracy.medium identity.[0, 0] 1.0 "Identity at [0,0]"
        floatClose Accuracy.medium identity.[1, 1] 1.0 "Identity at [1,1]"
        floatClose Accuracy.medium identity.[0, 1] 0.0 "Identity at [0,1]"
        floatClose Accuracy.medium identity.[1, 0] 0.0 "Identity at [1,0]"

    [<Fact>]
    let ``Inverse: matrix with fractional values`` () =
        let A = matrix [[0.5; 0.25]; [0.75; 0.5]]
        let inv = A.Inverse()

        // Verify A * A^-1 = I
        let identity = Matrix.multiply A inv
        floatClose Accuracy.medium identity.[0, 0] 1.0 "Identity at [0,0]"
        floatClose Accuracy.medium identity.[1, 1] 1.0 "Identity at [1,1]"
        floatClose Accuracy.medium identity.[0, 1] 0.0 "Identity at [0,1]"
        floatClose Accuracy.medium identity.[1, 0] 0.0 "Identity at [1,0]"
