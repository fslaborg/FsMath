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

