namespace FsMath.Tests.Vector

open System
open Xunit
open FsMath

open FsMath.Tests
open FsMath.Tests.AssertHelpers

open FsMath.Tests.ExpectoStyle

module VectorManipulationTests =


    [<Fact>]
    let ``foldi applies a function with index to a vector`` () =
        let v = [|1; 2; 3|]
        let result = Vector.foldi (fun i acc x -> acc + i * x) 0 v
        Assert.Equal(8, result)

    [<Fact>]
    let ``mapi applies a function with index to each element`` () =
        let v = [|1; 2; 3|]
        let result = Vector.mapi (fun i x -> x + i) v
        Assert.Equal<int[]>([|1; 3; 5|], result)

    [<Fact>]
    let ``filter filters elements by predicate`` () =
        let v = [|1; 2; 3; 4|]
        let result = Vector.filter (fun x -> x % 2 = 0) v
        Assert.Equal<int[]>([|2; 4|], result)

    [<Fact>]
    let ``init creates a vector using a generator function`` () =
        let result = Vector.init 5 (fun i -> i * 2)
        Assert.Equal<int[]>([|0; 2; 4; 6; 8|], result)

    [<Fact>]
    let ``slice extracts a slice from a vector`` () =
        let v = [|1; 2; 3; 4; 5|]
        let result = Vector.slice 1 3 v
        Assert.Equal<int[]>([|2; 3; 4|], result)

    [<Fact>]
    let ``argmax finds the index of the maximum value`` () =
        let v = [|1; 3; 2|]
        let result = Vector.argmax v
        Assert.Equal(1, result)

    [<Fact>]
    let ``argmin finds the index of the minimum value`` () =
        let v = [|3; 1; 2|]
        let result = Vector.argmin v
        Assert.Equal(1, result)

    [<Fact>]
    let ``padRight pads a vector to a given length`` () =
        let v = [|1; 2|]
        let result = Vector.padRight 5 0 v
        Assert.Equal<int[]>([|1; 2; 0; 0; 0|], result)

    [<Fact>]
    let ``zip combines two vectors with indices`` () =
        let v1 = [|1; 2; 3|]
        let v2 = [|4; 5; 6|]
        let result = Vector.zip v1 v2
        Assert.Equal<array<int*int>>([|(1, 4); (2, 5); (3, 6)|], result)

    [<Fact>]
    let ``argminBy finds the index of the minimum value using a projection`` () =
        let v = [|1; 2; 3|]
        let result = Vector.argminBy (fun x -> -x) v
        Assert.Equal(2, result)

    [<Fact>]
    let ``argmaxBy finds the index of the maximum value using a projection`` () =
        let v = [|1; 2; 3|]
        let result = Vector.argmaxBy (fun x -> -x) v
        Assert.Equal(0, result)

    [<Fact>]
    let ``tryFindIndex finds the index of the first matching element`` () =
        let v = [|1; 2; 3|]
        let result = Vector.tryFindIndex (fun x -> x = 2) v
        Assert.Equal(Some 1, result)

    [<Fact>]
    let ``findIndex finds the index of the first matching element or throws`` () =
        let v = [|1; 2; 3|]
        let result = Vector.findIndex (fun x -> x = 2) v
        Assert.Equal(1, result)

    //[<Fact>]
    //let ``enumerateNonZero enumerates non-zero elements with indices`` () =
    //    let v = [|0; 1; 0; 2|]
    //    let result = Vector.enumerateNonZero

