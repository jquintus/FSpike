﻿module UsefulCombiners
open System
open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote

module StartingPoint =
    /// Type that represents Success/Failure in parsing
    type Result<'a> =
        | Success of 'a
        | Failure of string

    /// Type that wraps a parsing function
    type Parser<'T> = Parser of (string -> Result<'T * string>)

    /// Parse a single character
    let pchar charToMatch =
        // define a nested inner function
        let innerFn str =
            if String.IsNullOrEmpty(str) then
                Failure "No more input"
            else
                let first = str.[0]
                if first = charToMatch then
                    let remaining = str.[1..]
                    Success (charToMatch,remaining)
                else
                    let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                    Failure msg
        // return the "wrapped" inner function
        Parser innerFn

    /// Run a parser with some input
    let run parser input =
        // unwrap parser to get inner function
        let (Parser innerFn) = parser
        // call inner function with input
        innerFn input

    /// Combine two parsers as "A andThen B"
    let andThen parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input

            // test the result for Failure/Success
            match result1 with
            | Failure err ->
                // return error from parser1
                Failure err

            | Success (value1,remaining1) ->
                // run parser2 with the remaining input
                let result2 =  run parser2 remaining1

                // test the result for Failure/Success
                match result2 with
                | Failure err ->
                    // return error from parser2
                    Failure err

                | Success (value2,remaining2) ->
                    // combine both values as a pair
                    let newValue = (value1,value2)
                    // return remaining input after parser2
                    Success (newValue,remaining2)

        // return the inner function
        Parser innerFn

    /// Infix version of andThen
    let ( .>>. ) = andThen

    /// Combine two parsers as "A orElse B"
    let orElse parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input

            // test the result for Failure/Success
            match result1 with
            | Success result ->
                // if success, return the original result
                result1

            | Failure err ->
                // if failed, run parser2 with the input
                let result2 = run parser2 input

                // return parser2's result
                result2

        // return the inner function
        Parser innerFn

    /// Infix version of orElse
    let ( <|> ) = orElse

    /// Choose any of a list of parsers
    let choice listOfParsers =
        List.reduce ( <|> ) listOfParsers

    /// Choose any of a list of characters
    let anyOf listOfChars =
        listOfChars
        |> List.map pchar // convert into parsers
        |> choice

module Section_1  =
    // =============================================
    // Section 1 - Map
    // =============================================
    open StartingPoint

    let parseDigit = anyOf ['0'..'9']

    // val parseThreeDigits: Parser<(char * char) * char>
    let parseThreeDigits =
        parseDigit .>>. parseDigit .>>. parseDigit

    // val mapP: ('a -> 'b) -> Parser<'a> -> Parser<'b>
    let mapP f parser =
        let innerFn input =
            let result = run parser input
            match result with
            | Success (value, remaining) ->
                let newValue = f value
                Success (newValue, remaining)
            | Failure err ->
                Failure err

        Parser innerFn

    let ( <!> ) = mapP
    let ( |>> ) x f = mapP f x

    // val parse3DigitsAsStr: Parse<string>
    let parse3DigitsAsStr =
        let transformTuple ((c1, c2), c3) =
            String [| c1; c2; c3; |]
        mapP transformTuple parseThreeDigits

    let parse3DigitsAsStr' =
        parseThreeDigits
        |>> fun((c1, c2), c3) -> String [| c1; c2; c3; |]

    // val parse3DigitsAsInt: Parse<int>
    let parse3DigitsAsInt =
        parse3DigitsAsStr
        |>> fun str -> int str

    // ----------------------------------------------------------------
    let run = StartingPoint.run
    let testCases  = 
        [
            testCase "parseThreeDigits 911A" <| fun _ ->
                test <@ run parseThreeDigits  "911A" = Success ((('9', '1'), '1'), "A") @>
            testCase "parse3DigitsAsStr 911A" <| fun _ ->
                test <@ run parse3DigitsAsStr "911A" = Success ("911", "A") @>
            testCase "parse3DigitsAsStr' 911A" <| fun _ ->
                test <@ run parse3DigitsAsStr' "911A" =  Success ("911", "A") @>
            testCase "parse3DigitsAsInt 911A" <| fun _ -> 
                test <@ run parse3DigitsAsInt "911A" = Success (911, "A") @>
        ]

module Section_2  =
    // =============================================
    // Section 2 - apply and return -> lifting functions to the Parser world
    // =============================================
    open StartingPoint
    open Section_1
    open Fuchu
    open Swensen.Unquote.Assertions
    open Swensen.Unquote


    // val returnP: 'a -> Parser<'a>
    let returnP x =
        let innerFn input =
            Success (x, input)
        Parser innerFn
    
    // val Parser<( 'a -> 'b ) -> Parser<'a> -> Parser<'b>
    let applyP fP xP =
        // create a parser containing a pair (f,x)
        // and then map the pair by applying f to x
        (fP .>>. xP)
        |> mapP (fun (f, x) -> f x)

    let ( <*> ) = applyP

    // val lift2: f:('a -> 'b -> 'c) -> Parser<'a> -> Parser<'b> -> Parser<'c'>
    let lift2 f xP yP = 
        returnP f <*> xP <*> yP
    
    let lift3 f xP yP zP =
        returnP f <*> xP <*> yP <*> zP
    
    // ----------------------------------------------------------------

    // val addP -> Parser<int> -> Parser<int> -> Parser<int>
    let addP = lift2 (+) 

    // val startsWith: string -> string -> bool
    let startsWith (str:string) prefix = 
        str.StartsWith(prefix)
    
    // val startsWithP: Parser<string> -> Parser<string> -> Parser<bool>
    let startsWithP = lift2 startsWith 

    // ----------------------------------------------------------------
    let parseDigitAsInt= anyOf ['0'..'9'] |>> fun char -> int char - int '0'

    let run = StartingPoint.run
    let testCases  = 
        [
            testCase "returnP 5" <| fun _ ->
                let fiveParser = returnP 5
                test <@ run fiveParser "anything" = Success (5, "anything") @>

            testCase "returnP 'Hello'" <| fun _ ->
                let helloParser = returnP "hello"
                test <@ run helloParser "xyz" = Success ("hello", "xyz") @>

            testCase "addP '2345' " <| fun _ ->
                let addFirstTwoInts = addP parseDigitAsInt parseDigitAsInt
                test <@ run addFirstTwoInts "2345" = Success(5, "45") @>
            
            testCase "lift3 - triple adds" <| fun _ ->
                let add3P = lift3 (fun x y z -> x + y + z)
                let addFirstThreeInts = add3P parseDigitAsInt parseDigitAsInt parseDigitAsInt
                test <@ run addFirstThreeInts "2345" = Success(9, "5") @>
        ]

module Test =
    // -----------------------------------------------------------------------
    // Run The Tests
    // -----------------------------------------------------------------------
    let tests = Seq.concat [
                 Seq.ofList Section_1.testCases
                 Seq.ofList Section_2.testCases
    ]

    [<Tests>] let parserTests = testList "UsefulCombiners" tests
