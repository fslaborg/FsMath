namespace FsMath.Tests.Vector

open System
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle

module VectorFloatTests =



    [<Fact>]
    let ``add: float vectors of same length`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = Vector.add v1 v2
        let expected = [| 5.0; 7.0; 9.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``add: throws on dimension mismatch`` () =
        let v1 = [| 1.0; 2.0 |]
        let v2 = [| 1.0; 2.0; 3.0 |]
        throws<ArgumentException>(fun () -> Vector.add v1 v2 |> ignore)

    [<Fact>]
    let ``subtract: float vectors of same length`` () =
        let v1 = [| 5.5; 5.0; 5.75 |]
        let v2 = [| 1.5; 2.0; 3.25 |]
        let result = Vector.subtract v1 v2
        let expected = [| 4.0; 3.0; 2.5 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``mean: computes mean of a float vector`` () =
        let v = [| 2.0; 4.0; 6.0; 8.0 |]
        let result = Vector.mean v
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``product: multiplies all elements of a float vector`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.product v
        floatEqual 6.0 result 1e-10

    [<Fact>]
    let ``dot: computes dot product of two float vectors`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = Vector.dot v1 v2
        floatEqual 32.0 result 1e-10

    [<Fact>]
    let ``norm: computes Euclidean norm of a 3-4-5 triangle (float)`` () =
        let v = [| 3.0; 4.0 |]
        let result = Vector.norm v
        floatEqual 5.0 result 1e-10


//  ------------------------------------------------------------------
/// OPERATOR TESTS (Float)
module VectorOperatorTests =


    [<Fact>]
    let ``vector-vector operators (float): .+ .- .* ./`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]

        let addResult = v1 .+ v2
        Assert.Equal<Vector<float>>([|5.0; 7.0; 9.0|], addResult)

        let subResult = v1 .- v2
        Assert.Equal<Vector<float>>([|-3.0; -3.0; -3.0|], subResult)

        let mulResult = v1 .* v2
        Assert.Equal<Vector<float>>([|4.0; 10.0; 18.0|], mulResult)

        let divResult = [| 10.0; 20.0; 30.0 |] ./ [| 2.0; 5.0; 5.0 |]
        Assert.Equal<Vector<float>>([|5.0; 4.0; 6.0|], divResult)

    [<Fact>]
    let ``vector-scalar operators (float): .+ .- .* ./`` () =
        let v = [| 2.5; 4.0; 6.0 |]

        let addScalarResult = v .+ 2.0
        Assert.Equal<Vector<float>>([|4.5; 6.0; 8.0|], addScalarResult)

        let subScalarResult = v .- 0.5
        Assert.Equal<Vector<float>>([|2.0; 3.5; 5.5|], subScalarResult)

        let mulScalarResult = v .* 1.5
        Assert.Equal<Vector<float>>([|3.75; 6.0; 9.0|], mulScalarResult)

        let divScalarResult = [| 10.0; 20.0; 30.0 |] ./ 10.0
        Assert.Equal<Vector<float>>([|1.0; 2.0; 3.0|], divScalarResult)
        
