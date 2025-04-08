namespace FsMath

open System

open System.Runtime.InteropServices

// let blockSize =
//     match sizeof<'T> with
//     | 4 -> 32  // float32 or int
//     | 8 -> 16  // float64
//     | _ -> 16  // fallback


/// Matrix type to hold values of a matrix in a 1D Arrays (Flattened Representation)
type Matrix<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (rows: int, cols: int, data: 'T[]) =

    /// Exposes the raw underlying data array (row-major flattened).
    member _.Data = data

    /// Number of rows in the matrix.
    member _.NumRows = rows

    /// Number of columns in the matrix.
    member _.NumCols = cols

    /// Matrix indexer for getting and setting elements at (i, j).
    member this.Item
        with get (i, j) =
            if i < 0 || i >= rows || j < 0 || j >= cols then
                invalidArg "index" $"Index out of range: ({i}, {j})"
            data.[i * cols + j]
        and set (i, j) value =
            if i < 0 || i >= rows || j < 0 || j >= cols then
                invalidArg "index" $"Index out of range: ({i}, {j})"
            data.[i * cols + j] <- value


    static member inline map2Unchecked<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (fVec: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (fScalar: 'T -> 'T -> 'T)
                (a: Matrix<'T>)
                (b: Matrix<'T>)
                : Matrix<'T> =

        // if a.Rows <> b.Rows || a.Cols <> b.Cols then
        //     invalidArg "b" "Matrix dimensions must match"

        let len = a.Data.Length
        let result = Array.zeroCreate<'T> len

        let simdSize = Numerics.Vector<'T>.Count
        let simdCount = len / simdSize
        let tailStart = simdCount * simdSize

        // Cast arrays to Vector<'T> spans
        let aVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(a.Data.AsSpan())
        let bVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(b.Data.AsSpan())
        let rVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

        for i = 0 to simdCount - 1 do
            rVec.[i] <- fVec aVec.[i] bVec.[i]

        for i = tailStart to len - 1 do
            result.[i] <- fScalar a.Data.[i] b.Data.[i]

        Matrix(a.NumRows, a.NumCols, result)


    static member inline mapScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (f: 'T -> 'T -> 'T)
                (matrix: Matrix<'T>)
                (scalar: 'T)
                : Matrix<'T> =

        let length = matrix.Data.Length
        let result = Array.zeroCreate<'T> length

        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize

        let scalarVec = Numerics.Vector<'T>(scalar)

        let mSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(matrix.Data.AsSpan())
        let rSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

        // SIMD chunk-wise operation
        for i = 0 to slotCount - 1 do
            rSpan.[i] <- fv mSpan.[i] scalarVec

        // Scalar fallback for remainder
        for i = ceiling to length - 1 do
            result.[i] <- f matrix.Data.[i] scalar

        Matrix(matrix.NumRows, matrix.NumCols, result)



     /// Creates a new matrix by initializing each element with a function `f(row, col)`.
    static member inline transpose<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (rows : int) 
                (cols : int)
                (data: 'T[]) 
                (blockSize: int) =

        //let blockSize = defaultArg blockSize 16

        let src = data
        let dst = Array.zeroCreate<'T> (rows * cols)

        let vectorSize = Numerics.Vector<'T>.Count

        // Process the matrix in blocks
        for i0 in 0 .. blockSize .. rows - 1 do
            for j0 in 0 .. blockSize .. cols - 1 do

                let iMax = min (i0 + blockSize) rows
                let jMax = min (j0 + blockSize) cols

                for i in i0 .. iMax - 1 do
                    let srcOffset = i * cols
                    for j in j0 .. jMax - 1 do
                        let v = src.[srcOffset + j]
                        dst.[j * rows + i] <- v

        dst

     member this.Transpose() =
        let blocksize = 16
        Matrix(this.NumCols, this.NumRows, Matrix.transpose this.NumRows this.NumCols this.Data blocksize)
     
    static member Init<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (rows: int)
                (cols: int)
                (f: int -> int -> 'T) =
        let data = Array.init (rows * cols) (fun idx -> f (idx / cols) (idx % cols))
        Matrix(rows, cols, data)

    static member Create<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (rows: int)
                (cols: int)
                (data: 'T[]) : Matrix<'T> =
        
        Matrix(rows, cols, data)


    /// Constructs a matrix from a 2D array.
    static member From2DArray<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (arr2D: 'T[,]) =
        let rows = arr2D.GetLength(0)
        let cols = arr2D.GetLength(1)
        let length = rows * cols

        let flat = Array.zeroCreate<'T> length
        let sourceSpan = MemoryMarshal.CreateSpan(&arr2D.[0, 0], length)
        sourceSpan.CopyTo(flat.AsSpan())

        Matrix(rows, cols, flat)

    /// Converts this matrix into a 2D array.
    member this.To2DArray() =
        let arr2D = Array2D.zeroCreate<'T> this.NumRows this.NumCols
        let targetSpan = MemoryMarshal.CreateSpan(&arr2D.[0, 0], this.NumRows * this.NumCols)
        this.Data.AsSpan().CopyTo(targetSpan)
        arr2D

    
    /// Constructs a matrix from a jagged array (`'T[][]`), assuming a rectangular structure.
    static member FromJaggedArray<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (jagged: 'T[][]) =
        let rows = jagged.Length
        let cols = if rows > 0 then jagged.[0].Length else 0

        // Optional safety check for rectangular shape
        for row in jagged do
            if row.Length <> cols then
                invalidArg "jagged" "All rows in jagged array must have the same length."

        let flat = Array.zeroCreate<'T> (rows * cols)

        for i = 0 to rows - 1 do
            let source = jagged.[i].AsSpan()
            let target = flat.AsSpan(i * cols, cols)
            source.CopyTo(target)

        Matrix(rows, cols, flat)

    /// Converts this matrix into a jagged array (`'T[][]`).
    member this.ToJaggedArray() =
        let result = Array.zeroCreate<'T[]> this.NumRows
        let dataSpan = this.Data.AsSpan()

        for i = 0 to this.NumRows - 1 do
            let row = Array.zeroCreate<'T> this.NumCols
            dataSpan.Slice(i * this.NumCols, this.NumCols).CopyTo(row.AsSpan())
            result.[i] <- row

        result


    /// Pretty string representation with formatting and truncation, using StringBuilder.
    /// Supports optional scientific notation for floating point types.
    member this.ToFormattedString(?maxRows: int, ?maxCols: int, ?floatFormat: string, ?useScientific: bool) =
        let maxRows = defaultArg maxRows 10
        let maxCols = defaultArg maxCols 10
        let useScientific = defaultArg useScientific false
        let floatFormat = 
            match floatFormat with
            | Some f -> f
            | None -> if useScientific then "0.##E+0" else "0.##"

        if this.NumRows = 0 || this.NumCols = 0 then
            "Matrix (empty)"
        else
            let rowsToShow = min this.NumRows maxRows
            let colsToShow = min this.NumCols maxCols

            // Format a single cell
            let formatCell (value: 'T) =
                match box value with
                | :? float as f    -> f.ToString(floatFormat)
                | :? float32 as f  -> f.ToString(floatFormat)
                | _                -> string value

            let formattedCells =
                Array.init rowsToShow (fun i ->
                    Array.init colsToShow (fun j ->
                        formatCell this.[i, j]
                    )
                )

            let colWidths =
                Array.init colsToShow (fun j ->
                    Array.init rowsToShow (fun i -> formattedCells.[i].[j].Length)
                    |> Array.max
                )

            let divider =
                colWidths
                |> Array.map (fun w -> String.replicate w "─")
                |> String.concat "─┼─"

            let sb = Text.StringBuilder()
            sb.AppendLine($"Matrix {this.NumRows}x{this.NumCols}:") |> ignore
            sb.AppendLine("┌ " + divider + " ┐") |> ignore

            for i = 0 to rowsToShow - 1 do
                sb.Append("│ ") |> ignore
                for j = 0 to colsToShow - 1 do
                    let cell = formattedCells.[i].[j].PadLeft(colWidths.[j])
                    sb.Append(cell) |> ignore
                    if j < colsToShow - 1 then sb.Append(" │ ") |> ignore
                sb.AppendLine(" │") |> ignore

            if this.NumRows > rowsToShow || this.NumCols > colsToShow then
                sb.AppendLine($"│ ... truncated ({this.NumRows}x{this.NumCols}) │") |> ignore

            sb.AppendLine("└ " + divider + " ┘") |> ignore
            sb.ToString()


    /// Default string representation (truncated, human-readable)
    override this.ToString() = this.ToFormattedString()

    static member inline checkSameShape<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (a: Matrix<'T>) (b: Matrix<'T>) =
        if a.NumRows <> b.NumRows || a.NumCols <> b.NumCols then
            invalidArg "b" $"Matrix dimensions must match. A is {a.NumRows}x{a.NumCols}, B is {b.NumRows}x{b.NumCols}"

    /// Element-wise addition
    static member inline add<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        let data = 
            SIMDUtils.map2Unchecked (+) (+) a.Data b.Data
        Matrix(a.NumRows, a.NumCols, data)  

    static member inline subtract<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        Matrix.map2Unchecked (-) (-) a b



    static member inline multiply<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType> 
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        Matrix.map2Unchecked (*) (*) a b


    static member inline divide<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType> 
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        Matrix.map2Unchecked (/) (/) a b


    static member inline addScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (m: Matrix<'T>) (scalar: 'T) : Matrix<'T> =
        
        Matrix.mapScalar (+) (+) m scalar



    static member inline subtractScalar<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (scalar: 'T) : Matrix<'T> =

        Matrix.mapScalar (-) (-) m scalar


    static member inline multiplyScalar<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (scalar: 'T) : Matrix<'T> =

        Matrix.mapScalar (*) (*) m scalar


    static member inline divideScalar<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (scalar: 'T) : Matrix<'T> =

        Matrix.mapScalar (/) (/) m scalar


    /// Standard matrix-vector product
    static member inline muliplyVector<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (v: Vector<'T>) : Vector<'T> =

        if m.NumCols <> v.Length then
            invalidArg "v" $"Vector length {v.Length} must match matrix column count {m.NumCols}"

        let result: Vector<'T> = Array.zeroCreate m.NumRows
        let zero = LanguagePrimitives.GenericZero<'T>

        let simdSize = Numerics.Vector<'T>.Count
        let vSpan = v.AsSpan()
        let vSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(vSpan)

        let tailStart = (v.Length / simdSize) * simdSize

        for row = 0 to m.NumRows - 1 do
            let rowOffset = row * m.NumCols
            let rowSpan = m.Data.AsSpan(rowOffset, m.NumCols)
            let rowSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(rowSpan)

            let mutable acc = Numerics.Vector<'T>(zero)

            for i = 0 to vSimd.Length - 1 do
                acc <- acc + rowSimd.[i] * vSimd.[i]

            let mutable tail = zero
            for i = tailStart to m.NumCols - 1 do
                tail <- tail + m.Data.[rowOffset + i] * v.[i]

            result.[row] <- Numerics.Vector.Sum(acc) + tail

        result

    static member inline addRowVector<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (v: Vector<'T>) : Matrix<'T> =

        if v.Length <> m.NumCols then
            invalidArg "v" $"Row vector length {v.Length} must match matrix column count {m.NumCols}"

        let resultData = Array.zeroCreate<'T> (m.NumRows * m.NumCols)
        let simdSize = Numerics.Vector<'T>.Count
        let simdCount = m.NumCols / simdSize
        let scalarStart = simdCount * simdSize

        let vSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v.AsSpan())

        for row = 0 to m.NumRows - 1 do
            let offset = row * m.NumCols
            let srcSpan = m.Data.AsSpan(offset, m.NumCols)
            let dstSpan = resultData.AsSpan(offset, m.NumCols)

            let srcSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(srcSpan)
            let dstSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(dstSpan)

            // SIMD part
            for i = 0 to simdCount - 1 do
                dstSimd.[i] <- srcSimd.[i] + vSimd.[i]

            // Tail (scalar)
            for i = scalarStart to m.NumCols - 1 do
                dstSpan.[i] <- srcSpan.[i] + v.[i]

        Matrix(m.NumRows, m.NumCols, resultData)


    static member inline addColVector<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (v: Vector<'T>) : Matrix<'T> =

        if v.Length <> m.NumRows then
            invalidArg "v" $"Column vector length {v.Length} must match matrix row count {m.NumRows}"

        let cols = m.NumCols
        let resultData = Array.zeroCreate<'T> (m.NumRows * cols)
        let simdSize = Numerics.Vector<'T>.Count

        for row = 0 to m.NumRows - 1 do
            let rowOffset = row * cols
            let srcRow = m.Data.AsSpan(rowOffset, cols)
            let dstRow = resultData.AsSpan(rowOffset, cols)

            let simdRow = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(srcRow)
            let simdDst = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(dstRow)

            let broadcast = Numerics.Vector<'T>(v.[row])
            let simdCount = cols / simdSize
            let scalarStart = simdCount * simdSize

            // SIMD-add: row + broadcast vector
            for i = 0 to simdCount - 1 do
                simdDst.[i] <- simdRow.[i] + broadcast

            // Scalar tail
            for i = scalarStart to cols - 1 do
                dstRow.[i] <- srcRow.[i] + v.[row]

        Matrix(m.NumRows, cols, resultData)

    // Matrix - matrix operations
    static member inline ( + ) (a: Matrix<'T>, b: Matrix<'T>) = Matrix.add a b
    static member inline ( - ) (a: Matrix<'T>, b: Matrix<'T>) = Matrix.subtract a b
    static member inline ( * ) (a: Matrix<'T>, b: Matrix<'T>) = Matrix.multiply a b
    static member inline ( / ) (a: Matrix<'T>, b: Matrix<'T>) = Matrix.divide a b

    // Matrix - vector operations
    static member inline ( * ) (m: Matrix<'T>, v: Vector<'T>) = Matrix.muliplyVector m v
    // static member inline ( + ) (m: Matrix<'T>, colVector: Vector<'T>) = Matrix.addColVector m colVector
    // static member inline ( +| ) (m: Matrix<'T>, rowVector: Vector<'T>) = Matrix.addRowVector m rowVector

    // Matrix - scalar operations
    static member inline (+) (m: Matrix<'T>, s: 'T) = Matrix.addScalar m s
    static member inline (+) (s: 'T, m: Matrix<'T>) = Matrix.addScalar m s

    static member inline (-) (m: Matrix<'T>, s: 'T) = Matrix.subtractScalar m s
    static member inline (-) (s: 'T, m: Matrix<'T>) = Matrix.subtractScalar m s

    static member inline (*) (m: Matrix<'T>, s: 'T) = Matrix.multiplyScalar m s
    static member inline (*) (s: 'T, m: Matrix<'T>) = Matrix.multiplyScalar m s

    static member inline (/) (m: Matrix<'T>, s: 'T) = Matrix.divideScalar m s
    static member inline (/) (s: 'T, m: Matrix<'T>) = Matrix.divideScalar m s



    static member inline diagonal<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (diag: Vector<'T>) : Matrix<'T> =

        let n = diag.Length
        let data = Array.zeroCreate<'T> (n * n)
        for i = 0 to n - 1 do
            data.[i * n + i] <- diag.[i]
        Matrix(n, n, data)


    static member inline identity<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (n: int) : Matrix<'T> =

        let data = Array.zeroCreate<'T> (n * n)
        for i = 0 to n - 1 do
            data.[i * n + i] <- 'T.One
        Matrix(n, n, data)

    static member inline zero<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (rows: int) 
        (cols: int) : Matrix<'T> =

        let data = Array.zeroCreate<'T> (rows * cols)
        Matrix(rows, cols, data)

    static member inline ones<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (rows: int) 
        (cols: int) : Matrix<'T> =

        let size = rows * cols
        let data = Array.zeroCreate<'T> size
        let simdSize = Numerics.Vector<'T>.Count
        let simdCount = size / simdSize

        let vOne = Numerics.Vector<'T>('T.One)
        let span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(data.AsSpan())

        for i = 0 to simdCount - 1 do
            span.[i] <- vOne

        for i = simdCount * simdSize to size - 1 do
            data.[i] <- 'T.One

        Matrix(rows, cols, data)

    static member inline getRow<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct 
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (i: int) : Vector<'T> =

        if i < 0 || i >= m.NumRows then
            invalidArg "i" $"Row index out of bounds: {i}"

        let rowSpan = m.Data.AsSpan(i * m.NumCols, m.NumCols)
        let result = Array.zeroCreate<'T> m.NumCols
        rowSpan.CopyTo(result.AsSpan())
        result

    static member inline getRows (m: Matrix<'T>) : Vector<'T>[] =
        Array.init m.NumRows (fun i -> Matrix.getRow m i)


    static member inline getCol<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct 
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (j: int) : Vector<'T> =

        if j < 0 || j >= m.NumCols then
            invalidArg "j" $"Column index out of bounds: {j}"

        let result = Array.zeroCreate<'T> m.NumRows
        let mutable offset = j
        for i = 0 to m.NumRows - 1 do
            result.[i] <- m.Data.[offset]
            offset <- offset + m.NumCols

        result

    static member inline getCols<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct 
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) : Vector<'T>[] =

        Array.init m.NumCols (fun j ->
            let col = Array.zeroCreate<'T> m.NumRows
            let mutable offset = j
            for i = 0 to m.NumRows - 1 do
                col.[i] <- m.Data.[offset]
                offset <- offset + m.NumCols
            col
        )



    // static member MatrixMultiply
    
    //     (a: Matrix<'T>, b: Matrix<'T>, ?blockSize: int) : Matrix<'T>
    //     when 'T : struct
    //     and 'T : (new : unit -> 'T)
    //     and 'T :> INumber<'T>
    //     and 'T :> ValueType =

    //     if a.Cols <> b.Rows then
    //         invalidArg "b" "Matrix dimensions must agree: a.Cols must equal b.Rows."

    //     let blockSize = defaultArg blockSize 32

    //     let m = a.Rows
    //     let n = b.Cols
    //     let k = a.Cols

    //     let result = Array.zeroCreate<'T> (m * n)
    //     let aData = a.Data
    //     let bData = b.Data
    //     let rData = result

    //     let vSize = Vector<'T>.Count

    //     // Blocked GEMM
    //     for i0 in 0 .. blockSize .. m - 1 do
    //         let iMax = min (i0 + blockSize) m
    //         for j0 in 0 .. blockSize .. n - 1 do
    //             let jMax = min (j0 + blockSize) n
    //             for k0 in 0 .. blockSize .. k - 1 do
    //                 let kMax = min (k0 + blockSize) k

    //                 // Iterate over block
    //                 for i in i0 .. iMax - 1 do
    //                     for j in j0 .. jMax - 1 do
    //                         let mutable sum = 'T.Zero
    //                         let mutable kk = k0

    //                         // SIMD accumulation
    //                         while kk <= kMax - vSize do
    //                             let aVec = Vector<'T>(aData, i * k + kk)
    //                             let bVec = Vector<'T>([| for offset in 0 .. vSize - 1 -> bData.[(kk + offset) * n + j] |])
    //                             sum <- sum + Vector.Dot(aVec, bVec)
    //                             kk <- kk + vSize

    //                         // Scalar tail
    //                         while kk < kMax do
    //                             sum <- sum + aData.[i * k + kk] * bData.[kk * n + j]
    //                             kk <- kk + 1

    //                         rData.[i * n + j] <- rData.[i * n + j] + sum
