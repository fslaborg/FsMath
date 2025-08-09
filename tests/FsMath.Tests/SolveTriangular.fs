namespace FsMath.Tests.LinearAlgebraTests

open System
open Xunit
open FsMath
open FsMath.Algebra

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle

module Matrices =

    // Define matrices for testing triangular linear systems
    // Upper triangular matrices

    let KDiagonal1 =
        [|
            [|1.;0.;0.|]
            [|0.;1.;0.|]
            [|0.;0.;1.|]
        |] |> Matrix.ofJaggedArray

    let KUpper1 =
        [|
            [|1.;1.;1.|]
            [|0.;1.;1.|]
            [|0.;0.;1.|]
        |] |> Matrix.ofJaggedArray

    let KUpperNeg1 =
        [|
            [|-1.;-1.;-1.|]
            [|0.;-1.;-1.|]
            [|0.;0.;-1.|]
        |] |> Matrix.ofJaggedArray

    let KUpperInf =
        [|
            [|infinity;infinity;infinity|]
            [|0.;infinity;infinity|]
            [|0.;0.;infinity|]
        |] |> Matrix.ofJaggedArray

    let KUpperNegInf =
        [|
            [|-infinity;-infinity;-infinity|]
            [|0.;-infinity;-infinity|]
            [|0.;0.;-infinity|]
        |] |> Matrix.ofJaggedArray

    let KUpperNaN =
        [|
            [|nan;nan;nan|]
            [|0.;nan;nan|]
            [|0.;0.;nan|]
        |] |> Matrix.ofJaggedArray

    let B1 =
        [|
            [|1.;1.;1.|]
            [|1.;1.;1.|]
            [|1.;1.;1.|]
        |] |> Matrix.ofJaggedArray

    let BNeg1 =
        [|
            [|-1.;-1.;-1.|]
            [|-1.;-1.;-1.|]
            [|-1.;-1.;-1.|]
        |] |> Matrix.ofJaggedArray

    let BInf =
        [|
            [|infinity;infinity;infinity|]
            [|infinity;infinity;infinity|]
            [|infinity;infinity;infinity|]
        |] |> Matrix.ofJaggedArray

    let BNegInf =
        [|
            [|-infinity;-infinity;-infinity|]
            [|-infinity;-infinity;-infinity|]
            [|-infinity;-infinity;-infinity|]
        |] |> Matrix.ofJaggedArray

    let BNaN =
        [|
            [|nan;nan;nan|]
            [|nan;nan;nan|]
            [|nan;nan;nan|]
        |] |> Matrix.ofJaggedArray

    // Lower triangular matrices for testing lower triangular systems

    let KLower1 =
        [|
            [|1.;1.;1.|]
            [|0.;1.;1.|]
            [|0.;0.;1.|]
        |] |> Matrix.ofJaggedArray |> Matrix.transpose

    let KLowerNeg1 =
        [|
            [|-1.;-1.;-1.|]
            [|0.;-1.;-1.|]
            [|0.;0.;-1.|]
        |] |> Matrix.ofJaggedArray |> Matrix.transpose

    let KLowerInf =
        [|
            [|infinity;infinity;infinity|]
            [|0.;infinity;infinity|]
            [|0.;0.;infinity|]
        |] |> Matrix.ofJaggedArray |> Matrix.transpose

    let KLowerNaN =
        [|
            [|nan;nan;nan|]
            [|0.;nan;nan|]
            [|0.;0.;nan|]
        |] |> Matrix.ofJaggedArray |> Matrix.transpose


    // Define matrices for testing triangular linear systems

    let b1 = vector [|1.;1.;1.|]

    let KLowerNegInf =
        [|
            [|-infinity;-infinity;-infinity|]
            [|0.;-infinity;-infinity|]
            [|0.;0.;-infinity|]
        |] |> Matrix.ofJaggedArray |> Matrix.transpose



module SolveTriangularLinearSystemsUpperTests =

    [<Fact>]
    let ``Diagonal 1 with Matrix 1 returns Matrix 1`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KDiagonal1 Matrices.B1 false
        let expected = Matrices.B1
        floatMatrixClose Accuracy.high actual expected "Should be 3x3 Matrix of 1"


    [<Fact>]
    let ``Upper 1 with Matrix 1 returns 1 in last row`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KUpper1 Matrices.B1 false
        let expected =
            matrix [|
                [|0.;0.;0.|];
                [|0.;0.;0.|];
                [|1.;1.;1.|]
            |]
        floatMatrixClose Accuracy.high actual expected "Should be 3x3 Matrix with 1 in last row"
        

    [<Fact>]
    let ``Upper -1 with Matrix 1 returns -1 in last row`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KUpperNeg1 Matrices.B1 false
        let expected =
            matrix [|
                [|0.;0.;0.|];
                [|0.;0.;0.|];
                [|-1.;-1.;-1.|]
            |]
        floatMatrixClose Accuracy.high actual expected "Should be 3x3 Matrix with -1 in last row"
       

    [<Fact>]
    let ``Upper Inf with Matrix 1 returns 0 in last row and NaN elsewhere`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KUpperInf Matrices.B1 false
        let expected =
            matrix [|
                [|nan;nan;nan|];
                [|nan;nan;nan|];
                [|0.;0.;0.|]
            |]
        floatMatrixClose Accuracy.high actual expected
            "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
        

    [<Fact>]
    let ``Upper NaN with Matrix 1 returns all NaN`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KUpperNaN Matrices.B1 false
        floatMatrixClose Accuracy.high actual Matrices.BNaN 
            "Should be 3x3 Matrix of NaN"


module SolveTriangularLinearSystemsLowerTests =

    [<Fact>]
    let ``Diagonal 1 with Matrix 1 returns Matrix 1 (lower)`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KDiagonal1 Matrices.B1 true
        let expected = Matrices.B1
        floatMatrixClose Accuracy.high actual expected
            "Should be 3x3 Matrix of 1"

    [<Fact>]
    let ``Lower 1 with Matrix 1 returns 1 in first row`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KLower1 Matrices.B1 true
        let expected =
            matrix [|
                [|1.;1.;1.|];
                [|0.;0.;0.|];
                [|0.;0.;0.|]
            |]
        floatMatrixClose Accuracy.high actual expected
            "Should be 3x3 Matrix with 1 in first row"
        

    [<Fact>]
    let ``Lower -1 with Matrix 1 returns -1 in first row`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KLowerNeg1 Matrices.B1 true
        let expected =
            matrix [|
                [|-1.;-1.;-1.|];
                [|0.;0.;0.|];
                [|0.;0.;0.|]
            |]
        floatMatrixClose Accuracy.high actual expected
            "Should be 3x3 Matrix with -1 in first row"

    [<Fact>]
    let ``Lower Inf with Matrix 1 returns 0 in first row and NaN elsewhere`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KLowerInf Matrices.B1 true
        let expected =
            matrix [|
                [|0.;0.;0.|];
                [|nan;nan;nan|];
                [|nan;nan;nan|]
            |]
        floatMatrixClose Accuracy.high actual expected
            "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
        

    [<Fact>]
    let ``Lower NaN with Matrix 1 returns all NaN`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystems Matrices.KLowerNaN Matrices.B1 true
        let expected =
            matrix [|
                [|nan;nan;nan|];
                [|nan;nan;nan|];
                [|nan;nan;nan|]
            |]
        floatMatrixClose Accuracy.high actual expected
            "Should be 3x3 Matrix of NaN"
  
module SolveTriangularLinearSystemUpperTests =

    [<Fact>]
    let ``Upper realistic example with vector`` () =
        let a =
            [|
                [|1.;2.;3.|]
                [|0.;1.;1.|]
                [|0.;0.;2.|]
            |] |> Matrix.ofJaggedArray
        let b = vector [|8.;4.;2.|]
        let expected = [|-1.;3.;1.|]

        let actual = LinearAlgebra.solveTriangularLinearSystem a b false
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of -1.;3.;1."
       

    [<Fact>]
    let ``Diagonal 1 with Vector 1 returns Vector 1`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KDiagonal1 Matrices.b1 false
        let expected = [|1.;1.;1.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 1"
        

    [<Fact>]
    let ``Upper 1 with Vector 1 returns [0;0;1]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KUpper1 Matrices.b1 false
        let expected = [|0.;0.;1.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 0 0 1"
        

    [<Fact>]
    let ``Upper -1 with Vector 1 returns [0;0;-1]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KUpperNeg1 Matrices.b1 false
        let expected = [|0.;0.;-1.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 0 0 -1"
        

    [<Fact>]
    let ``Upper Inf with Vector 1 returns [NaN;NaN;0]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KUpperInf Matrices.b1 false
        let expected = [|nan;nan;0.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of NaN NaN 0"
        

    [<Fact>]
    let ``Upper -Inf with Vector 1 returns [NaN;NaN;0]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KUpperNegInf Matrices.b1 false
        let expected = [|nan;nan;0.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of NaN NaN 0"
        

    [<Fact>]
    let ``Upper NaN with Vector 1 returns [NaN;NaN;NaN]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KUpperNaN Matrices.b1 false
        let expected = [|nan;nan;nan|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of NaN"


module SolveTriangularLinearSystemLowerTests =

    [<Fact>]
    let ``Lower realistic example with vector`` () =
        let a =
            [|
                [|1.;0.;0.|];
                [|2.;1.;0.|];
                [|3.;1.;2.|]
            |] |> Matrix.ofJaggedArray
        let b = vector [|8.;4.;2.|]
        let expected = [|8.;-12.;-5.|]

        let actual = LinearAlgebra.solveTriangularLinearSystem a b true
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 8.;-12.;-5."
        

    [<Fact>]
    let ``Diagonal 1 with Vector 1 returns Vector 1 (lower)`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KDiagonal1 Matrices.b1 true
        let expected = [|1.;1.;1.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 1"
        

    [<Fact>]
    let ``Lower 1 with Vector 1 returns [1;0;0]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KLower1 Matrices.b1 true
        let expected = [|1.;0.;0.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 1 0 0"
        

    [<Fact>]
    let ``Lower -1 with Vector 1 returns [-1;0;0]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KLowerNeg1 Matrices.b1 true
        let expected = [|-1.;0.;0.|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of -1 0 0"
        

    [<Fact>]
    let ``Lower Inf with Vector 1 returns [0;NaN;NaN]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KLowerInf Matrices.b1 true
        let expected = [|0.;nan;nan|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 0 NaN NaN"
        

    [<Fact>]
    let ``Lower -Inf with Vector 1 returns [0;NaN;NaN]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KLowerNegInf Matrices.b1 true
        let expected = [|0.;nan;nan|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of 0 NaN NaN"
        

    [<Fact>]
    let ``Lower NaN with Vector 1 returns [NaN;NaN;NaN]`` () =
        let actual = LinearAlgebra.solveTriangularLinearSystem Matrices.KLowerNaN Matrices.b1 true
        let expected = [|nan;nan;nan|]
        floatVectorClose Accuracy.high actual expected
            "Should be Vector of NaN"
       


        