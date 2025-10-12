namespace FsMath.Tests.VectorModule

open System
open Xunit
open FsMath
open FsMath.Tests.AssertHelpers

module VectorModuleTests =

    [<Fact>]
    let ``foldi: performs indexed fold`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let result = Vector.foldi (fun i acc x -> acc + float i * x) 0.0 v
        // 0*1 + 1*2 + 2*3 + 3*4 = 0 + 2 + 6 + 12 = 20
        floatEqual 20.0 result 1e-10

    [<Fact>]
    let ``foldi: works with empty-like case`` () =
        let v = [| 5.0 |]
        let result = Vector.foldi (fun i acc x -> acc + x) 0.0 v
        floatEqual 5.0 result 1e-10

    [<Fact>]
    let ``mapi: applies function with index`` () =
        let v = [| 10.0; 20.0; 30.0 |]
        let result = Vector.mapi (fun i x -> x + float i) v
        let expected = [| 10.0; 21.0; 32.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``filter: filters elements by predicate`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = Vector.filter (fun x -> x > 2.5) v
        let expected = [| 3.0; 4.0; 5.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``filter: returns empty when nothing matches`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.filter (fun x -> x > 10.0) v
        Assert.Empty(result)

    [<Fact>]
    let ``init: creates vector from generator function`` () =
        let result = Vector.init 5 (fun i -> float i * 2.0)
        let expected = [| 0.0; 2.0; 4.0; 6.0; 8.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``slice: extracts a portion of vector`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = Vector.slice 1 3 v
        let expected = [| 2.0; 3.0; 4.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``argmax: finds index of maximum value`` () =
        let v = [| 1.0; 5.0; 3.0; 7.0; 2.0 |]
        let result = Vector.argmax v
        Assert.Equal(3, result)

    [<Fact>]
    let ``argmax: returns first index when multiple maxima`` () =
        let v = [| 1.0; 5.0; 5.0; 3.0 |]
        let result = Vector.argmax v
        Assert.Equal(1, result)

    [<Fact>]
    let ``argmin: finds index of minimum value`` () =
        let v = [| 5.0; 1.0; 3.0; 0.5; 2.0 |]
        let result = Vector.argmin v
        Assert.Equal(3, result)

    [<Fact>]
    let ``argmin: returns first index when multiple minima`` () =
        let v = [| 3.0; 1.0; 1.0; 5.0 |]
        let result = Vector.argmin v
        Assert.Equal(1, result)

    [<Fact>]
    let ``padRight: pads vector to longer length`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.padRight 6 0.0 v
        let expected = [| 1.0; 2.0; 3.0; 0.0; 0.0; 0.0 |]
        floatArrayClose expected result 1e-10

    [<Fact>]
    let ``padRight: returns original vector when already long enough`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.padRight 2 0.0 v
        Assert.Same(v, result)

    [<Fact>]
    let ``zip: combines two vectors into tuple array`` () =
        let v1 = [| 1.0; 2.0; 3.0 |]
        let v2 = [| 4.0; 5.0; 6.0 |]
        let result = Vector.zip v1 v2
        Assert.Equal((1.0, 4.0), result.[0])
        Assert.Equal((2.0, 5.0), result.[1])
        Assert.Equal((3.0, 6.0), result.[2])

    [<Fact>]
    let ``argminBy: finds index of minimum projected value`` () =
        let v = [| 3.0; -5.0; 2.0; -8.0 |]
        let result = Vector.argminBy abs v
        Assert.Equal(2, result)  // abs(2.0) = 2.0 is minimum

    [<Fact>]
    let ``argmaxBy: finds index of maximum projected value`` () =
        let v = [| 3.0; -5.0; 2.0; -8.0 |]
        let result = Vector.argmaxBy abs v
        Assert.Equal(3, result)  // abs(-8.0) = 8.0 is maximum

    [<Fact>]
    let ``tryFindIndex: finds first matching element`` () =
        let v = [| 1.0; 3.0; 5.0; 7.0 |]
        let result = Vector.tryFindIndex (fun x -> x > 4.0) v
        Assert.Equal(Some 2, result)

    [<Fact>]
    let ``tryFindIndex: returns None when no match`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.tryFindIndex (fun x -> x > 10.0) v
        Assert.Equal(None, result)

    [<Fact>]
    let ``findIndex: finds first matching element`` () =
        let v = [| 1.0; 3.0; 5.0; 7.0 |]
        let result = Vector.findIndex (fun x -> x > 4.0) v
        Assert.Equal(2, result)

    [<Fact>]
    let ``findIndex: throws when no match`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<Exception>(fun () -> Vector.findIndex (fun x -> x > 10.0) v |> ignore) |> ignore

    [<Fact>]
    let ``enumerateNonZero: extracts non-zero elements with indices`` () =
        let v = [| 0.0; 2.0; 0.0; 4.0; 5.0 |]
        let result = Vector.enumerateNonZero v
        Assert.Equal(3, result.Length)
        Assert.Equal((1, 2.0), result.[0])
        Assert.Equal((3, 4.0), result.[1])
        Assert.Equal((4, 5.0), result.[2])

    [<Fact>]
    let ``enumerateNonZero: returns empty for all zeros`` () =
        let v = [| 0.0; 0.0; 0.0 |]
        let result = Vector.enumerateNonZero v
        Assert.Empty(result)

    [<Fact>]
    let ``split: splits vector at index`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let left, right = Vector.split 2 v
        floatArrayClose [| 1.0; 2.0 |] left 1e-10
        floatArrayClose [| 3.0; 4.0; 5.0 |] right 1e-10

    [<Fact>]
    let ``split: handles split at beginning`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let left, right = Vector.split 0 v
        Assert.Empty(left)
        floatArrayClose [| 1.0; 2.0; 3.0 |] right 1e-10

    [<Fact>]
    let ``chunk: splits vector into chunks`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0 |]
        let result = Vector.chunk 3 v
        Assert.Equal(3, result.Length)
        floatArrayClose [| 1.0; 2.0; 3.0 |] result.[0] 1e-10
        floatArrayClose [| 4.0; 5.0; 6.0 |] result.[1] 1e-10
        floatArrayClose [| 7.0 |] result.[2] 1e-10

    [<Fact>]
    let ``windowed: creates sliding windows`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = Vector.windowed 3 v
        Assert.Equal(3, result.Length)
        floatArrayClose [| 1.0; 2.0; 3.0 |] result.[0] 1e-10
        floatArrayClose [| 2.0; 3.0; 4.0 |] result.[1] 1e-10
        floatArrayClose [| 3.0; 4.0; 5.0 |] result.[2] 1e-10

    [<Fact>]
    let ``windowed: returns empty when window too large`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.windowed 5 v
        Assert.Empty(result)

    [<Fact>]
    let ``splitVector: splits by indices`` () =
        let v = [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
        let indices = [| 1; 3 |]
        let selected, rest = Vector.splitVector indices v
        floatArrayClose [| 20.0; 40.0 |] selected 1e-10
        floatArrayClose [| 10.0; 30.0; 50.0 |] rest 1e-10

    [<Fact>]
    let ``splitVector: handles unsorted indices`` () =
        let v = [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
        let indices = [| 3; 1 |]  // unsorted
        let selected, rest = Vector.splitVector indices v
        floatArrayClose [| 20.0; 40.0 |] selected 1e-10
        floatArrayClose [| 10.0; 30.0; 50.0 |] rest 1e-10

    [<Fact>]
    let ``ofSeq: converts sequence to vector`` () =
        let seq = seq { 1.0; 2.0; 3.0 }
        let result = Vector.ofSeq seq
        floatArrayClose [| 1.0; 2.0; 3.0 |] result 1e-10

    [<Fact>]
    let ``pow: raises each element to power`` () =
        let v = [| 2.0; 3.0; 4.0 |]
        let result = Vector.pow 2.0 v
        floatArrayClose [| 4.0; 9.0; 16.0 |] result 1e-10

    [<Fact>]
    let ``sub: extracts subvector from index to end`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let result = Vector.sub 2 v
        floatArrayClose [| 3.0; 4.0; 5.0 |] result 1e-10

    [<Fact>]
    let ``sub: throws on negative index`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> Vector.sub -1 v |> ignore) |> ignore

    [<Fact>]
    let ``sub: throws on index out of bounds`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        Assert.Throws<ArgumentException>(fun () -> Vector.sub 5 v |> ignore) |> ignore

    [<Fact>]
    let ``sub: returns empty vector when starting at end`` () =
        let v = [| 1.0; 2.0; 3.0 |]
        let result = Vector.sub 3 v
        Assert.Empty(result)

    // Tests for permuteBy function
    [<Fact>]
    let ``permuteBy: applies identity permutation`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let P = Permutation.identity
        let result = Vector.permuteBy P v
        floatArrayClose [| 1.0; 2.0; 3.0; 4.0 |] result 1e-10

    [<Fact>]
    let ``permuteBy: applies simple permutation`` () =
        // Permutation: [2, 0, 1] means result[0] = v[2], result[1] = v[0], result[2] = v[1]
        let v = [| 10.0; 20.0; 30.0 |]
        let P = Permutation.ofArray [| 2; 0; 1 |]
        let result = Vector.permuteBy P v
        floatArrayClose [| 30.0; 10.0; 20.0 |] result 1e-10

    [<Fact>]
    let ``permuteBy: applies swap permutation`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let P = Permutation.swap 1 3  // Swap indices 1 and 3
        let result = Vector.permuteBy P v
        floatArrayClose [| 1.0; 4.0; 3.0; 2.0; 5.0 |] result 1e-10

    [<Fact>]
    let ``permuteBy: applies reversal permutation`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        let P = Permutation.reversal 5
        let result = Vector.permuteBy P v
        floatArrayClose [| 5.0; 4.0; 3.0; 2.0; 1.0 |] result 1e-10

    [<Fact>]
    let ``permuteBy: applies rotation permutation`` () =
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let P = Permutation.rotation 4 1  // Rotate right by 1
        let result = Vector.permuteBy P v
        floatArrayClose [| 2.0; 3.0; 4.0; 1.0 |] result 1e-10

    [<Fact>]
    let ``permuteBy: works with single element`` () =
        let v = [| 42.0 |]
        let P = Permutation.identity
        let result = Vector.permuteBy P v
        floatArrayClose [| 42.0 |] result 1e-10

    [<Fact>]
    let ``permuteBy: double permutation equals inverse`` () =
        // Applying a permutation twice with its inverse should return original
        let v = [| 1.0; 2.0; 3.0; 4.0 |]
        let pArray = [| 2; 0; 3; 1 |]
        let P = Permutation.ofArray pArray
        let Pinv = Permutation.inverse 4 P
        let permuted = Vector.permuteBy P v
        let restored = Vector.permuteBy Pinv permuted
        floatArrayClose v restored 1e-10

    [<Fact>]
    let ``permuteBy: works with integers`` () =
        let v = [| 10; 20; 30; 40 |]
        let P = Permutation.ofArray [| 3; 2; 1; 0 |]
        let result = Vector.permuteBy P v
        let expected = [| 40; 30; 20; 10 |]
        Assert.Equal<int[]>(expected, result)
