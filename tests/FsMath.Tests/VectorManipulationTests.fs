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

    [<Fact>]
    let ``enumerateNonZero enumerates non-zero elements with indices`` () =
        let v = [|0; 1; 0; 2|]
        let result = Vector.enumerateNonZero v
        Assert.Equal<array<int*int>>([|(1, 1); (3, 2)|], result)

    [<Fact>]
    let ``enumerateNonZero returns empty for all-zero vector`` () =
        let v = [|0; 0; 0|]
        let result = Vector.enumerateNonZero v
        Assert.Equal<array<int*int>>([||], result)

    [<Fact>]
    let ``split divides vector at given index`` () =
        let v = [|1; 2; 3; 4; 5|]
        let left, right = Vector.split 2 v
        Assert.Equal<int[]>([|1; 2|], left)
        Assert.Equal<int[]>([|3; 4; 5|], right)

    [<Fact>]
    let ``split at beginning returns empty prefix`` () =
        let v = [|1; 2; 3|]
        let left, right = Vector.split 0 v
        Assert.Equal<int[]>([||], left)
        Assert.Equal<int[]>([|1; 2; 3|], right)

    [<Fact>]
    let ``split at end returns empty suffix`` () =
        let v = [|1; 2; 3|]
        let left, right = Vector.split 3 v
        Assert.Equal<int[]>([|1; 2; 3|], left)
        Assert.Equal<int[]>([||], right)

    [<Fact>]
    let ``chunk divides vector into equally sized pieces`` () =
        let v = [|1; 2; 3; 4; 5; 6; 7|]
        let result = Vector.chunk 3 v
        Assert.Equal(3, result.Length)
        Assert.Equal<int[]>([|1; 2; 3|], result.[0])
        Assert.Equal<int[]>([|4; 5; 6|], result.[1])
        Assert.Equal<int[]>([|7|], result.[2])

    [<Fact>]
    let ``chunk with size larger than vector returns single chunk`` () =
        let v = [|1; 2; 3|]
        let result = Vector.chunk 10 v
        Assert.Equal(1, result.Length)
        Assert.Equal<int[]>([|1; 2; 3|], result.[0])

    [<Fact>]
    let ``windowed creates sliding windows`` () =
        let v = [|1; 2; 3; 4; 5|]
        let result = Vector.windowed 3 v
        Assert.Equal(3, result.Length)
        Assert.Equal<int[]>([|1; 2; 3|], result.[0])
        Assert.Equal<int[]>([|2; 3; 4|], result.[1])
        Assert.Equal<int[]>([|3; 4; 5|], result.[2])

    [<Fact>]
    let ``windowed with size larger than vector returns empty`` () =
        let v = [|1; 2|]
        let result = Vector.windowed 5 v
        Assert.Equal<int[][]>([||], result)

    [<Fact>]
    let ``splitVector extracts elements at indices`` () =
        let v = [|10; 20; 30; 40; 50|]
        let nvi, nv = Vector.splitVector [|1; 3|] v
        Assert.Equal<int[]>([|20; 40|], nvi)
        Assert.Equal<int[]>([|10; 30; 50|], nv)

    [<Fact>]
    let ``splitVector with no indices returns all in second part`` () =
        let v = [|1; 2; 3|]
        let nvi, nv = Vector.splitVector [||] v
        Assert.Equal<int[]>([||], nvi)
        Assert.Equal<int[]>([|1; 2; 3|], nv)

    [<Fact>]
    let ``splitVector with all indices returns all in first part`` () =
        let v = [|1; 2; 3|]
        let nvi, nv = Vector.splitVector [|0; 1; 2|] v
        Assert.Equal<int[]>([|1; 2; 3|], nvi)
        Assert.Equal<int[]>([||], nv)

    [<Fact>]
    let ``permuteBy reorders vector according to permutation`` () =
        let v = [|10; 20; 30|]
        let perm = Permutation.ofArray [|2; 0; 1|]
        let result = Vector.permuteBy perm v
        Assert.Equal<int[]>([|30; 10; 20|], result)

    [<Fact>]
    let ``ofSeq converts sequence to vector`` () =
        let seq = seq { 1; 2; 3 }
        let result = Vector.ofSeq seq
        Assert.Equal<int[]>([|1; 2; 3|], result)

    [<Fact>]
    let ``pow raises each element to specified power`` () =
        let v = [|2.0; 3.0; 4.0|]
        let result = Vector.pow 2.0 v
        Assert.Equal<float[]>([|4.0; 9.0; 16.0|], result)

    [<Fact>]
    let ``sub extracts subvector from offset to end`` () =
        let v = [|1; 2; 3; 4; 5|]
        let result = Vector.sub 2 v
        Assert.Equal<int[]>([|3; 4; 5|], result)

    [<Fact>]
    let ``sub with offset 0 returns entire vector`` () =
        let v = [|1; 2; 3|]
        let result = Vector.sub 0 v
        Assert.Equal<int[]>([|1; 2; 3|], result)

    [<Fact>]
    let ``sub at end returns empty vector`` () =
        let v = [|1; 2; 3|]
        let result = Vector.sub 3 v
        Assert.Equal<int[]>([||], result)

    [<Fact>]
    let ``sub with negative offset throws`` () =
        let v = [|1; 2; 3|]
        throws<ArgumentException>(fun () -> Vector.sub -1 v |> ignore)

    [<Fact>]
    let ``sub with offset beyond length throws`` () =
        let v = [|1; 2; 3|]
        throws<ArgumentException>(fun () -> Vector.sub 10 v |> ignore)

    [<Fact>]
    let ``tryFindIndex returns None when element not found`` () =
        let v = [|1; 2; 3|]
        let result = Vector.tryFindIndex (fun x -> x = 10) v
        Assert.Equal(None, result)

    [<Fact>]
    let ``findIndex throws when element not found`` () =
        let v = [|1; 2; 3|]
        throws<exn>(fun () -> Vector.findIndex (fun x -> x = 10) v |> ignore)

    [<Fact>]
    let ``padRight does not pad when vector already long enough`` () =
        let v = [|1; 2; 3; 4; 5|]
        let result = Vector.padRight 3 0 v
        Assert.Equal<int[]>([|1; 2; 3; 4; 5|], result)

