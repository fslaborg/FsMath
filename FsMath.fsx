#r "./src/FsMath/bin/Release/net8.0/FsMath.dll"

open FsMath
open FsMath.Acceleration
open Vector.Operators

let a = vector [| 1.0; 2.0; 3.0 |]
let b = vector [| 4.0; 5.0; 6.0 |]

//SIMDVector.add a b 
// VectorOps.(+) a b 



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