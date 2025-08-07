namespace FsMath.Tests.Vector

open System
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle

module VectorIntTests =



    [<Fact>]
    let ``add: integer vectors of same length`` () =
        let v1 = [| 1; 2; 3 |]
        let v2 = [| 4; 5; 6 |]
        let result = Vector.add v1 v2
        let expected = [| 5; 7; 9 |]
        intArrayEqual expected result

    [<Fact>]
    let ``add: throws on dimension mismatch`` () =
        let v1 = [| 1; 2 |]
        let v2 = [| 1; 2; 3 |]
        throws<ArgumentException>(fun () -> Vector.add v1 v2 |> ignore)

    [<Fact>]
    let ``subtract: integer vectors of same length`` () =
        let v1 = [| 5; 5; 5 |]
        let v2 = [| 1; 2; 3 |]
        let result = Vector.subtract v1 v2
        let expected = [| 4; 3; 2 |]
        intArrayEqual expected result

    [<Fact>]
    let ``subtract: throws on dimension mismatch`` () =
        let v1 = [| 1; 2; 3 |]
        let v2 = [| 1; 2 |]
        throws<ArgumentException>(fun () -> Vector.subtract v1 v2 |> ignore)

    [<Fact>]
    let ``multiply: integer vectors of same length`` () =
        let v1 = [| 2; 2; 2 |]
        let v2 = [| 3; 4; 5 |]
        let result = Vector.multiply v1 v2
        let expected = [| 6; 8; 10 |]
        intArrayEqual expected result

    [<Fact>]
    let ``dot: computes dot product of two integer vectors`` () =
        let v1 = [| 1; 2; 3 |]
        let v2 = [| 4; 5; 6 |]
        let result = Vector.dot v1 v2
        Assert.Equal(32, result)

    [<Fact>]
    let ``dot: throws on dimension mismatch`` () =
        let v1 = [| 1; 2 |]
        let v2 = [| 3; 4; 5 |]
        throws<ArgumentException>(fun () -> Vector.dot v1 v2 |> ignore)

    [<Fact>]
    let ``sum: sums all elements of an integer vector`` () =
        let v = [| 1; 2; 3; 4 |]
        let result = Vector.sum v
        Assert.Equal(10, result)

    [<Fact>]
    let ``product: multiplies all elements of an integer vector`` () =
        let v = [| 1; 2; 3; 4 |]
        let result = Vector.product v
        Assert.Equal(24, result)

    [<Fact>]
    let ``max: finds maximum in a non-empty integer vector`` () =
        let v = [| 10; 2; 30; 4 |]
        let result = Vector.max v
        Assert.Equal(30, result)

    [<Fact>]
    let ``min: finds minimum in a non-empty integer vector`` () =
        let v = [| 10; 2; 30; 4 |]
        let result = Vector.min v
        Assert.Equal(2, result)
