#r "./src/FsMath/bin/Release/net8.0/FsMath.dll"

open System
open FsMath

SpecialFunctions.Gamma._gamma 0.5
let ea =SpecialFunctions.Gamma._gammaLn 420.

printf "%f" ea

// 🧪 Example usage
let v1: Vector<int> = [| 1; 2; 3 |]
let v2: Vector<int> = [| 4; 5; 6 |]
let scalar = 10

let r1 = v1 .+ v2    // [|5; 7; 9|]
let r2 = v1 .+ scalar // [|11; 12; 13|]
let r3 = scalar .+ v1 // [|11; 12; 13|]


printfn "v1 .+ v2    = %A" r1
printfn "v1 .+ 10    = %A" r2
printfn "10 .+ v1    = %A" r3


let a : Vector<float> =  [| 1.0; 2.0; 3.0 |]
let b : Vector<float> =  [| 4.0; 5.0; 6.0 |]

a .+ b

//

let m =    
    Matrix.Init 50 5 
        (fun i j -> 
            (float (i * 5 + j)))

1. + m

// Default ToString (10x10 max, float format "0.##")
printfn "%A" m

// Custom format (no truncation, 3 decimals)
printfn "%s" (m.ToFormattedString(maxRows = 100, maxCols = 100, floatFormat = "0.000"))