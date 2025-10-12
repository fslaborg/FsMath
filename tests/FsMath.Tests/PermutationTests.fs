namespace FsMath.Tests.Permutation

open System
open Xunit
open FsMath
open FsMath.Tests.ExpectoStyle

module PermutationTests =

    // ========================================
    // ofFreshArray tests
    // ========================================

    [<Fact>]
    let ``ofFreshArray: creates identity permutation from [0,1,2]`` () =
        let arr = [| 0; 1; 2 |]
        let p = Permutation.ofFreshArray arr
        Assert.Equal(0, p 0)
        Assert.Equal(1, p 1)
        Assert.Equal(2, p 2)

    [<Fact>]
    let ``ofFreshArray: creates valid permutation from [2,0,1]`` () =
        let arr = [| 2; 0; 1 |]
        let p = Permutation.ofFreshArray arr
        Assert.Equal(2, p 0)
        Assert.Equal(0, p 1)
        Assert.Equal(1, p 2)

    [<Fact>]
    let ``ofFreshArray: handles single element [0]`` () =
        let arr = [| 0 |]
        let p = Permutation.ofFreshArray arr
        Assert.Equal(0, p 0)

    [<Fact>]
    let ``ofFreshArray: handles empty array`` () =
        let arr = Array.empty<int>
        let p = Permutation.ofFreshArray arr
        // Empty permutation should be valid but not callable with any index
        // (would throw if called)
        Assert.NotNull(p)

    [<Fact>]
    let ``ofFreshArray: throws on duplicate indices`` () =
        let arr = [| 0; 1; 1 |]
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr |> ignore)

    [<Fact>]
    let ``ofFreshArray: throws on out-of-range negative index`` () =
        let arr = [| -1; 0; 1 |]
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr |> ignore)

    [<Fact>]
    let ``ofFreshArray: throws on out-of-range positive index`` () =
        let arr = [| 0; 1; 3 |]  // 3 is out of range for size 3
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr |> ignore)

    [<Fact>]
    let ``ofFreshArray: permutation function throws on out-of-range query`` () =
        let arr = [| 1; 0 |]
        let p = Permutation.ofFreshArray arr
        throws<ArgumentException>(fun () -> p 2 |> ignore)
        throws<ArgumentException>(fun () -> p (-1) |> ignore)

    // ========================================
    // ofArray tests
    // ========================================

    [<Fact>]
    let ``ofArray: creates valid permutation`` () =
        let arr = [| 1; 2; 0 |]
        let p = Permutation.ofArray arr
        Assert.Equal(1, p 0)
        Assert.Equal(2, p 1)
        Assert.Equal(0, p 2)

    [<Fact>]
    let ``ofArray: does not modify original array`` () =
        let arr = [| 1; 2; 0 |]
        let original = Array.copy arr
        let p = Permutation.ofArray arr
        // Ensure original array unchanged
        Assert.Equal<int[]>(original, arr)

    // ========================================
    // sign tests
    // ========================================

    [<Fact>]
    let ``sign: identity permutation has sign +1`` () =
        let p = Permutation.identity
        let s = Permutation.sign 3 p
        Assert.Equal(1.0, s)

    [<Fact>]
    let ``sign: single transposition has sign -1`` () =
        // Swap 0 and 1: [1, 0, 2]
        let p = Permutation.swap 0 1
        let s = Permutation.sign 3 p
        Assert.Equal(-1.0, s)

    [<Fact>]
    let ``sign: two transpositions has sign +1`` () =
        // [1, 0, 2] then swap 1 and 2 -> [1, 2, 0]
        let arr = [| 1; 2; 0 |]
        let p = Permutation.ofArray arr
        let s = Permutation.sign 3 p
        // This is a 3-cycle: 0->1->2->0, which is (n-1) = 2 transpositions, so sign is +1
        Assert.Equal(1.0, s)

    [<Fact>]
    let ``sign: reversal of size 2 has sign -1`` () =
        let p = Permutation.reversal 2
        let s = Permutation.sign 2 p
        Assert.Equal(-1.0, s)

    [<Fact>]
    let ``sign: reversal of size 3 has sign -1`` () =
        let p = Permutation.reversal 3
        let s = Permutation.sign 3 p
        // Reversal [2,1,0] has 1 cycle (0->2->0) and 1 fixed point (1),
        // so 3-2=1 transposition, sign = -1
        Assert.Equal(-1.0, s)

    // ========================================
    // ofPairs tests
    // ========================================

    [<Fact>]
    let ``ofPairs: creates permutation from pairs`` () =
        let p = Permutation.ofPairs [(0, 2); (2, 1)]
        Assert.Equal(2, p 0)
        Assert.Equal(1, p 2)

    [<Fact>]
    let ``ofPairs: unmapped indices map to themselves`` () =
        let p = Permutation.ofPairs [(0, 1)]
        Assert.Equal(1, p 0)
        Assert.Equal(2, p 2)  // unmapped
        Assert.Equal(5, p 5)  // unmapped

    [<Fact>]
    let ``ofPairs: empty pairs creates identity`` () =
        let p = Permutation.ofPairs []
        Assert.Equal(0, p 0)
        Assert.Equal(5, p 5)

    // ========================================
    // swap tests
    // ========================================

    [<Fact>]
    let ``swap: exchanges two indices`` () =
        let p = Permutation.swap 1 3
        Assert.Equal(3, p 1)
        Assert.Equal(1, p 3)

    [<Fact>]
    let ``swap: leaves other indices unchanged`` () =
        let p = Permutation.swap 1 3
        Assert.Equal(0, p 0)
        Assert.Equal(2, p 2)
        Assert.Equal(4, p 4)

    [<Fact>]
    let ``swap: swapping same index is identity`` () =
        let p = Permutation.swap 2 2
        Assert.Equal(2, p 2)

    // ========================================
    // reversal tests
    // ========================================

    [<Fact>]
    let ``reversal: size 1 reverses to itself`` () =
        let p = Permutation.reversal 1
        Assert.Equal(0, p 0)

    [<Fact>]
    let ``reversal: size 3 reverses indices`` () =
        let p = Permutation.reversal 3
        Assert.Equal(2, p 0)
        Assert.Equal(1, p 1)
        Assert.Equal(0, p 2)

    [<Fact>]
    let ``reversal: size 5 reverses indices`` () =
        let p = Permutation.reversal 5
        Assert.Equal(4, p 0)
        Assert.Equal(3, p 1)
        Assert.Equal(2, p 2)
        Assert.Equal(1, p 3)
        Assert.Equal(0, p 4)

    [<Fact>]
    let ``reversal: throws on non-positive size`` () =
        throws<ArgumentException>(fun () -> Permutation.reversal 0 |> ignore)
        throws<ArgumentException>(fun () -> Permutation.reversal -1 |> ignore)

    // ========================================
    // rotation tests
    // ========================================

    [<Fact>]
    let ``rotation: distance 0 is identity`` () =
        let p = Permutation.rotation 5 0
        Assert.Equal(0, p 0)
        Assert.Equal(1, p 1)
        Assert.Equal(4, p 4)

    [<Fact>]
    let ``rotation: positive distance rotates right`` () =
        let p = Permutation.rotation 5 2
        Assert.Equal(2, p 0)
        Assert.Equal(3, p 1)
        Assert.Equal(0, p 3)  // wraps around
        Assert.Equal(1, p 4)  // wraps around

    [<Fact>]
    let ``rotation: negative distance rotates left`` () =
        let p = Permutation.rotation 5 -1
        Assert.Equal(4, p 0)  // wraps around
        Assert.Equal(0, p 1)
        Assert.Equal(1, p 2)
        Assert.Equal(3, p 4)

    [<Fact>]
    let ``rotation: full rotation throws exception`` () =
        // This should throw because distance >= size
        throws<ArgumentException>(fun () -> Permutation.rotation 5 5 |> ignore)

    [<Fact>]
    let ``rotation: throws on non-positive size`` () =
        throws<ArgumentException>(fun () -> Permutation.rotation 0 1 |> ignore)
        throws<ArgumentException>(fun () -> Permutation.rotation -1 1 |> ignore)

    [<Fact>]
    let ``rotation: throws on distance >= size`` () =
        throws<ArgumentException>(fun () -> Permutation.rotation 5 5 |> ignore)
        throws<ArgumentException>(fun () -> Permutation.rotation 5 6 |> ignore)
        throws<ArgumentException>(fun () -> Permutation.rotation 5 -5 |> ignore)

    // ========================================
    // identity tests
    // ========================================

    [<Fact>]
    let ``identity: maps every index to itself`` () =
        let p = Permutation.identity
        Assert.Equal(0, p 0)
        Assert.Equal(1, p 1)
        Assert.Equal(100, p 100)
        Assert.Equal(-5, p -5)

    // ========================================
    // inverse tests
    // ========================================

    [<Fact>]
    let ``inverse: of identity is identity`` () =
        let p = Permutation.identity
        let pInv = Permutation.inverse 5 p
        Assert.Equal(0, pInv 0)
        Assert.Equal(1, pInv 1)
        Assert.Equal(4, pInv 4)

    [<Fact>]
    let ``inverse: of swap is itself`` () =
        let p = Permutation.swap 1 3
        let pInv = Permutation.inverse 5 p
        Assert.Equal(3, pInv 1)
        Assert.Equal(1, pInv 3)
        Assert.Equal(0, pInv 0)
        Assert.Equal(2, pInv 2)

    [<Fact>]
    let ``inverse: of [2,0,1] is [1,2,0]`` () =
        // p: 0->2, 1->0, 2->1
        // pInv: 0->1, 1->2, 2->0
        let arr = [| 2; 0; 1 |]
        let p = Permutation.ofArray arr
        let pInv = Permutation.inverse 3 p
        Assert.Equal(1, pInv 0)
        Assert.Equal(2, pInv 1)
        Assert.Equal(0, pInv 2)

    [<Fact>]
    let ``inverse: applying p then pInv returns identity`` () =
        let arr = [| 1; 2; 0; 4; 3 |]
        let p = Permutation.ofArray arr
        let pInv = Permutation.inverse 5 p

        // p(pInv(i)) should equal i for all i
        for i in 0..4 do
            Assert.Equal(i, p(pInv i))
            Assert.Equal(i, pInv(p i))

    [<Fact>]
    let ``inverse: throws on non-positive size`` () =
        let p = Permutation.identity
        throws<ArgumentException>(fun () -> Permutation.inverse 0 p |> ignore)
        throws<ArgumentException>(fun () -> Permutation.inverse -1 p |> ignore)

    // ========================================
    // Composition and algebraic properties
    // ========================================

    [<Fact>]
    let ``composition: two swaps`` () =
        let p1 = Permutation.swap 0 1
        let p2 = Permutation.swap 1 2
        let composed = fun i -> p2 (p1 i)

        // p1: 0->1, 1->0, 2->2
        // p2: 0->0, 1->2, 2->1
        // composed: 0->p2(1)=2, 1->p2(0)=0, 2->p2(2)=1
        Assert.Equal(2, composed 0)
        Assert.Equal(0, composed 1)
        Assert.Equal(1, composed 2)

    [<Fact>]
    let ``composition: rotation then reversal`` () =
        let p1 = Permutation.rotation 4 1  // [1,2,3,0]
        let p2 = Permutation.reversal 4    // [3,2,1,0]
        let composed = fun i -> p2 (p1 i)

        // p1(0)=1, p2(1)=2
        Assert.Equal(2, composed 0)
        // p1(1)=2, p2(2)=1
        Assert.Equal(1, composed 1)
        // p1(2)=3, p2(3)=0
        Assert.Equal(0, composed 2)
        // p1(3)=0, p2(0)=3
        Assert.Equal(3, composed 3)

    // ========================================
    // Additional edge cases for better coverage
    // ========================================

    [<Fact>]
    let ``ofFreshArray: validates all elements are unique (comprehensive check)`` () =
        // Test that validation catches duplicates at various positions
        let arr1 = [| 0; 0 |]  // Duplicate at positions 0 and 1
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr1 |> ignore)

        let arr2 = [| 0; 1; 2; 1 |]  // Duplicate 1 at positions 1 and 3
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr2 |> ignore)

        let arr3 = [| 2; 2; 0 |]  // Duplicate 2 at positions 0 and 1
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr3 |> ignore)

    [<Fact>]
    let ``ofFreshArray: validates range for every element`` () =
        // Test that out-of-range validation works for various positions
        let arr1 = [| -2; 0; 1 |]  // Negative at position 0
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr1 |> ignore)

        let arr2 = [| 0; 1; 5 |]  // Out of range at position 2 (size=3, max=2)
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr2 |> ignore)

        let arr3 = [| 0; 10; 2 |]  // Out of range at position 1
        throws<ArgumentException>(fun () -> Permutation.ofFreshArray arr3 |> ignore)

    [<Fact>]
    let ``permutation function validates index range on every call`` () =
        let p = Permutation.ofArray [| 1; 0; 2 |]

        // Test various out-of-range indices
        throws<ArgumentException>(fun () -> p -1 |> ignore)
        throws<ArgumentException>(fun () -> p -100 |> ignore)
        throws<ArgumentException>(fun () -> p 3 |> ignore)
        throws<ArgumentException>(fun () -> p 10 |> ignore)
        throws<ArgumentException>(fun () -> p 999 |> ignore)
