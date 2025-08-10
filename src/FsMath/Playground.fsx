#I "./bin/Release/net8.0"
#r "FsMath.dll"

//#r "nuget: Plotly.NET"

open FsMath
open FsMath.Algebra

1. + 1.0

[|1.|] + [|2.|]

let A = Matrix(3, 2, [| 1.0; 0.0;
                        1.0; 1.0;
                        0.0; 1.0 |])
let Q, _ = LinearAlgebra.qrModifiedGramSchmidt A
let QTQ = Matrix.matmul (Matrix.transpose Q) Q
let I : Matrix<float> = Matrix.identity 2



