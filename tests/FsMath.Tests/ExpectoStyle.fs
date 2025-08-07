namespace FsMath.Tests

open System
open FsMath

/// <summary>
/// Provides Expecto-style assertion helpers for use with xUnit tests.
/// </summary>
module ExpectoStyle =

    /// <summary>
    /// Represents accuracy thresholds for floating point comparisons.
    /// </summary>
    type Accuracy = {
        /// <summary>Maximum allowed absolute difference.</summary>
        absolute: float
        /// <summary>Maximum allowed relative difference, scaled by the larger of the two values.</summary>
        relative: float
    }

    /// <summary>
    /// Predefined accuracy levels for convenience.
    /// </summary>
    module Accuracy =
        /// <summary>Low accuracy: absolute = 1e-6, relative = 1e-3</summary>
        let low      = { absolute = 1e-6; relative = 1e-3 }

        /// <summary>Medium accuracy: absolute = 1e-8, relative = 1e-5</summary>
        let medium   = { absolute = 1e-8; relative = 1e-5 }

        /// <summary>High accuracy: absolute = 1e-10, relative = 1e-7</summary>
        let high     = { absolute = 1e-10; relative = 1e-7 }
        
        /// <summary>High accuracy: absolute = 1e-12, relative = 1e-9</summary>
        let veryHigh = { absolute = 1e-12; relative = 1e-9 }

    /// <summary>
    /// Asserts that two floating point numbers are approximately equal within the specified accuracy.
    /// </summary>
    /// <param name="accuracy">Accuracy thresholds for comparison.</param>
    /// <param name="actual">The actual value computed.</param>
    /// <param name="expected">The expected reference value.</param>
    /// <param name="message">A message to include on failure.</param>
    let floatClose (accuracy: Accuracy) (actual: float) (expected: float) (message: string) =
        let absDiff = abs (actual - expected)
        let relDiff = accuracy.relative * max (abs actual) (abs expected)
        let limit = accuracy.absolute + relDiff
        if absDiff > limit then
            failwithf "%s\nExpected: %g\nActual: %g\nAbs diff: %g > Allowed: %g"
                message expected actual absDiff limit

    /// <summary>
    /// Asserts that two floating point numbers are not approximately equal within the specified accuracy.
    /// </summary>
    /// <param name="accuracy">Accuracy thresholds for comparison.</param>
    /// <param name="actual">The actual value computed.</param>
    /// <param name="expected">The value it should not be close to.</param>
    /// <param name="message">A message to include on failure.</param>
    let floatNotClose (accuracy: Accuracy) (actual: float) (expected: float) (message: string) =
        let absDiff = abs (actual - expected)
        let relDiff = accuracy.relative * max (abs actual) (abs expected)
        let limit = accuracy.absolute + relDiff
        if absDiff <= limit then
            failwithf "%s\nExpected NOT close to %g, but was %g\nAbs diff: %g <= Allowed: %g"
                message expected actual absDiff limit

    /// <summary>
    /// Asserts that a boolean condition is true.
    /// </summary>
    /// <param name="condition">The condition to assert.</param>
    /// <param name="message">A message to include on failure.</param>
    let isTrue (condition: bool) (message: string) =
        if not condition then failwithf "Assertion failed: %s" message

    /// <summary>
    /// Asserts that a boolean condition is false.
    /// </summary>
    /// <param name="condition">The condition to assert.</param>
    /// <param name="message">A message to include on failure.</param>
    let isFalse (condition: bool) (message: string) =
        if condition then failwithf "Assertion failed (expected false): %s" message

    /// <summary>
    /// Asserts that two values are equal using structural equality.
    /// </summary>
    /// <param name="expected">The expected value.</param>
    /// <param name="actual">The actual value.</param>
    /// <param name="message">A message to include on failure.</param>
    let equal (expected: 'T) (actual: 'T) (message: string) =
        if expected <> actual then
            failwithf "%s\nExpected: %A\nActual: %A" message expected actual

    /// <summary>
    /// Asserts that two sequences are equal in content and order.
    /// </summary>
    /// <param name="expected">The expected sequence.</param>
    /// <param name="actual">The actual sequence.</param>
    /// <param name="message">A message to include on failure.</param>
    let sequenceEqual (expected: #seq<'T>) (actual: #seq<'T>) (message: string) =
        let eArr, aArr = Seq.toArray expected, Seq.toArray actual
        if eArr <> aArr then
            failwithf "%s\nExpected: %A\nActual: %A" message eArr aArr

    /// <summary>
    /// Asserts that a specific exception type is thrown.
    /// </summary>
    /// <typeparam name="'ex">The expected exception type.</typeparam>
    /// <param name="action">The function to execute.</param>
    /// <param name="message">A message to include on failure.</param>
    let throws<'ex when 'ex :> exn> (action: unit -> unit) (message: string) =
        try
            action()
            failwithf "%s\nExpected exception of type %s, but no exception was thrown."
                message typeof<'ex>.Name
        with
        | :? 'ex -> () // Expected, do nothing
        | ex ->
            failwithf "%s\nExpected exception of type %s, but got: %s"
                message typeof<'ex>.Name (ex.GetType().Name)


    let floatMatrixClose accuracy (A: Matrix<float>) (B: Matrix<float>) message =
        equal A.NumCols B.NumCols "Column count mismatch"
        equal A.NumRows B.NumRows "Row count mismatch"
        for i = 0 to A.NumRows - 1 do
            for j = 0 to A.NumCols - 1 do
                floatClose accuracy A.[i, j] B.[i, j] $"{message} at ({i},{j})"