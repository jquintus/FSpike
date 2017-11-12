open System

type Result<'a> = 
        | Success of 'a
        | Failure of string

type Parser<'T> = Parser of (string -> Result<'T * string>)

let pchar charToMatch = 
    let innerFn str = 
        if String.IsNullOrEmpty str then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Expecting %c but found %c" charToMatch first
                Failure msg
    Parser innerFn

let run parser input = 
    // unwrwap the parser to get to the inner function
    let (Parser innerFn) = parser
    innerFn input

// val andThen -> Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
let andThen parser1 parser2  = 
    let innerFn str = 
        let result1 = run parser1 str
        match result1 with
        | Failure err -> Failure err
        | Success (value1, remaining1) -> 
            let result2 = run parser2 remaining1
            match result2 with
            | Failure err -> Failure err
            | Success (value2, remaining2) -> 
                let newValue = (value1, value2)
                Success (newValue, remaining2)

    Parser innerFn

let parseA = pchar 'A'
let parseB = pchar 'B'

let parseAB = andThen parseA parseB

let testCases = [
    //   Test Name            Parser   str    Expected result
    ("run parseAB \"AB\"",   parseAB, "AB",   Success (('A', 'B'), ""))
    ("run parseAB \"ABC\"",  parseAB, "ABC",  Success (('A', 'B'), "C"))
    ("run parseAB \"AC\"",   parseAB, "AC",   Failure "Expecting B but found C")
    ("run parseAB \"A\"",    parseAB, "A",    Failure "No more input")
    ("run parseAB \"\"",     parseAB, "",     Failure "No more input")
]

let foo = run parseAB "AB"

