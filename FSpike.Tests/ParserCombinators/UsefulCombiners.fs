module UsefulCombiners
open System

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


    let testCases1 = [
        ("parseThreeDigits 911A",  parseThreeDigits,  "911A", Success ((('9', '1'), '1'), "A"))
    ]
    let testCases2 = [
        ("parse3DigitsAsStr 911A",  parse3DigitsAsStr,  "911A", Success ("911", "A"))
        ("parse3DigitsAsStr' 911A", parse3DigitsAsStr', "911A", Success ("911", "A"))
    ]
    let testCases3 = [
        ("parse3DigitsAsInt 911A",  parse3DigitsAsInt,  "911A", Success (911, "A"))
    ]

module Test = 
    // -----------------------------------------------------------------------
    // Run The Tests
    // -----------------------------------------------------------------------
    open Fuchu
    open StartingPoint
    
    let tests = Seq.concat [ 
                 (mapTests2 run id Section_1.testCases1) 
                 (mapTests2 run id Section_1.testCases2) 
    ]

    [<Tests>] let parserTests = testList "UsefulCombiners" tests