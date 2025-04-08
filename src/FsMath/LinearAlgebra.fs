namespace FsMath

open System

type LinearAlgebra =

    static member inline qrModifiedGramSchmidt<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
        (A: Matrix<'T>) : Matrix<'T> * Matrix<'T> =

        let m, n = A.NumRows, A.NumCols

        let r = Matrix.zero n n
        let qCols: Vector<'T>[] = Array.zeroCreate n
        let aCols = Matrix.getCols A

        for j = 0 to n - 1 do
            let v = Array.copy aCols.[j]

            for i = 0 to j - 1 do
                let qi = qCols.[i]
                let rij = Vector.dotProduct qi v
                r.[i, j] <- rij
                for k = 0 to m - 1 do
                    v.[k] <- v.[k] - rij * qi.[k]

            let norm = Vector.norm v 
            r.[j, j] <- norm
            let qj = Vector.divideScalar v norm
            qCols.[j] <- qj

        let qData = Array.zeroCreate (m * n)
        for j = 0 to n - 1 do
            for i = 0 to m - 1 do
                qData.[i * n + j] <- qCols.[j].[i]

        Matrix(m, n, qData), r


    static member inline backSubstitute<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (r: Matrix<'T>) 
        (y: Vector<'T>) : Vector<'T> =

        let n = r.NumRows

        if r.NumCols <> n || y.Length <> n then
            invalidArg "dimensions" "R must be square and match the length of y"

        let x = Array.zeroCreate<'T> n

        for i = n - 1 downto 0 do
            let mutable sum = y.[i]
            for j = i + 1 to n - 1 do
                sum <- sum - r.[i, j] * x.[j]
            x.[i] <- sum / r.[i, i]

        x





    static member inline solveLinearQR<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
        (A: Matrix<'T>) 
        (b: Vector<'T>) : Vector<'T> =

        if A.NumRows <> b.Length then
            invalidArg "b" "Vector length must match number of rows in matrix A"

        let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
        let Qt = Q.Transpose()
        let y = Matrix.muliplyVector Qt b
        LinearAlgebra.backSubstitute R y



    /// Solve K * X = B for X, where K is a triangular matrix (lower or upper).
    /// K is square of size n×n; B is n×m. Returns an n×m result X.
    static member inline solveTriangularLinearSystem
        (K       : Matrix<'T>)
        (B       : Matrix<'T>)
        (isLower : bool)
        : Matrix<'T> =
  
        /// Subtract `scaleVal * src[srcOffset..srcOffset+count-1]` from
        /// `dst[dstOffset..dstOffset+count-1]` in place.
        let inline subScaledRowInPlace
            (scaleVal   : 'T)
            (dstOffset  : int)
            (srcOffset  : int)
            (count      : int)
            (dst        : 'T[])
            (src        : 'T[]) =
        
            let scaleVec = Numerics.Vector<'T>(scaleVal)

            // Vector-level callback:  dstVec - (scaleVal * srcVec)
            let fv (dstVec: Numerics.Vector<'T>) (srcVec: Numerics.Vector<'T>) =
                dstVec - (scaleVec * srcVec)
        
            // Scalar fallback: d - (scaleVal * s)
            let f (d: 'T) (s: 'T) =
                d - (scaleVal * s)
        
            // Now call map2RangeInPlace with the new signature:
            //   map2RangeInPlace fv f dstStartIdx srcStartIdx count dst src
            SIMDUtils.map2RangeInPlace fv f dstOffset srcOffset count dst src

        /// Multiply `arr[offset..offset+count-1]` by `scaleVal` in place
        let inline scaleRowInPlace
            (scaleVal  : 'T)
            (offset    : int)
            (count     : int)
            (arr       : 'T[]) =
        
            let scaleVec = Numerics.Vector<'T>(scaleVal)

            // Vector-level callback: chunk * scaleVal
            let fv (chunk: Numerics.Vector<'T>) =
                chunk * scaleVec

            // Scalar fallback: x * scaleVal
            let f (x: 'T) = x * scaleVal

            // mapRangeInPlace fv f offset count arr
            SIMDUtils.mapRangeInPlace fv f offset count arr  

        // Check shape consistency
        let nK, mK = K.NumRows, K.NumCols
        let nB, mB = B.NumRows, B.NumCols
        if nK <> mK || nB <> nK then
            invalidArg "Matrix" "K must be square and B must have matching row count for K"

        // Copy B into a new matrix X (the solution)
        let X = Matrix<'T>(B.NumRows, B.NumCols, Array.copy B.Data)

        let n = nK  // size of the NxN triangular matrix K
        let m = mB  // # of columns in B (and thus X)

        let Kdata = K.Data
        let Xdata = X.Data

        // For row i, the contiguous slice in Xdata is [i*m .. i*m + m - 1]
        // => offset = i*m, length = m

        if isLower then
            // ------ Forward Substitution ------
            // for i in [0..n-1]:
            //   X[i,*] <- X[i,*] - Σ_{j=0..i-1} [K[i,j] * X[j,*]]
            //   then scale row i by 1/K[i,i]
            for i = 0 to n - 1 do
                let baseI = i * m
                // Subtract scaled rows for j in [0..i-1]
                for j = 0 to i - 1 do
                    let kij = Kdata.[i * n + j]  // K[i,j]
                    if kij <> 'T.Zero then
                        let baseJ = j * m
                        // subScaledRowInPlace scaleVal dstOffset srcOffset count dst src
                        subScaledRowInPlace
                            kij 
                            baseI  // row i offset in X
                            baseJ  // row j offset in X
                            m      // number of columns
                            Xdata  
                            Xdata
                // Divide row i by the diagonal K[i,i]
                let diag = Kdata.[i * n + i]
                let invDiag = 'T.One / diag
                // scaleRowInPlace scaleVal offset count arr
                scaleRowInPlace
                    invDiag
                    baseI
                    m
                    Xdata
        else
            // ------ Backward Substitution ------
            // for i in [n-1..0]:
            //   X[i,*] <- X[i,*] - Σ_{j=i+1..n-1} [K[i,j] * X[j,*]]
            //   then scale row i by 1/K[i,i]
            for i = n - 1 downto 0 do
                let baseI = i * m
                for j = i + 1 to n - 1 do
                    let kij = Kdata.[i * n + j]  // K[i,j]
                    if kij <> 'T.Zero then
                        let baseJ = j * m
                        subScaledRowInPlace
                            kij 
                            baseI
                            baseJ
                            m
                            Xdata
                            Xdata
                let diag = Kdata.[i * n + i]
                let invDiag = 'T.One / diag
                scaleRowInPlace
                    invDiag
                    baseI
                    m
                    Xdata

        X
