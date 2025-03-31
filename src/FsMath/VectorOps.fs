namespace FsMath

open System


module Operators =
    
    let inline (.+) (v1: Vector<'T>) ( v2: Vector<'T>) = Vector.add v1 v2
    //let inline (.+) (scalar: 'T) ( v: Vector<'T>) = SIMDVector.addScalar v scalar

    let inline (+.) (v: Vector<'T>) (scalar: 'T) = Vector.addScalar v scalar

    // type AddScalar =
    //     static member inline (+.) (v: Vector<'T>, scalar: 'T) = SIMDVector.addScalar v scalar

    // type Sub =
    //     static member inline (-) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.subtract v1 v2

    // type SubScalar =
    //     static member inline (-.) (v: Vector<'T>, scalar: 'T) = SIMDVector.subtractScalar v scalar

    // type Mul =
    //     static member inline ( * ) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.multiply v1 v2

    // type MulScalar =
    //     static member inline ( *. ) (v: Vector<'T>, scalar: 'T) = SIMDVector.multiplyScalar v scalar

    // type Div =
    //     static member inline (/) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.divide v1 v2

    // type DivScalar =
    //     static member inline (/.) (v: Vector<'T>, scalar: 'T) = SIMDVector.divideScalar v scalar

    // type Dot =
    //     static member inline ( .. ) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.dotProduct v1 v2



// static member inline (+) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.add v1 v2
// static member inline (+.) (v: Vector<'T>, scalar: ^T) = SIMDVector.addScalar v scalar
// static member inline (-) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.subtract v1 v2

// static member inline (-.) (v: Vector<'T>, scalar: ^T) = SIMDVector.subtractScalar v scalar


// static member inline ( * ) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.multiply v1 v2
// static member inline ( *. ) (v: Vector<'T>, scalar: ^T) = SIMDVector.multiplyScalar v scalar
// static member inline (/) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.divide v1 v2
// static member inline (/.) (v: Vector<'T>, scalar: ^T) = SIMDVector.divideScalar v scalar

// static member inline ( .. ) (v1: Vector<'T>, v2: Vector<'T>) = SIMDVector.dotProduct v1 v2

// static member inline (^.) (v: Vector<'T>, scalar: ^T) = SIMDMath.powAuto v scalar



// /// Element-wise vector addition
// static member inline add v1 v2 = SIMDVector.add v1 v2

// /// Add scalar to each element
// static member inline addScalar v scalar = SIMDVector.addScalar v scalar

// /// Element-wise vector subtraction
// static member inline subtract v1 v2 = SIMDVector.subtract v1 v2

// /// Subtract scalar from each element
// static member inline subtractScalar v scalar = SIMDVector.subtractScalar v scalar

// /// Element-wise vector multiplication
// static member inline multiply v1 v2 = SIMDVector.multiply v1 v2

// /// Multiply each element by scalar
// static member inline multiplyScalar v scalar = SIMDVector.multiplyScalar v scalar

// /// Element-wise vector division
// static member inline divide v1 v2 = SIMDVector.divide v1 v2

// /// Divide each element by scalar
// static member inline divideScalar v scalar = SIMDVector.divideScalar v scalar

// /// Dot product of two vectors
// static member inline dotProduct v1 v2 = SIMDVector.dotProduct v1 v2

// /// Raise each element to the power of a scalar
// //static member inline powScalar v scalar = SIMDMath.powAuto v scalar


