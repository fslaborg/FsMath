namespace FsMath

open System
open System.Runtime.InteropServices

[<AutoOpen>]
module MatrixExt =

    //[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    type Matrix<'T when 'T :> Numerics.INumber<'T>
                    and 'T : (new: unit -> 'T)
                    and 'T : struct
                    and 'T : comparison
                    and 'T :> ValueType> with
    
        member inline this.Inverse() = Algebra.LinearAlgebra.inverse this
        
        //member inline this.Determinant = Algebra.LinearAlgebra.determinant this