module CharParser

open System
open Fuchu
type Result<'a> = 
        | Success of 'a
        | Failure of string

module Section_1 = 
    // -----------------------------------------------------------------------
    // Parsing a hard-coded character: 
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/#implementation-1-parsing-a-hard-coded-character
    // -----------------------------------------------------------------------

    // val A_Parser : string -> bool * string
    let A_Parser str = 
        if String.IsNullOrEmpty(str) then
            false, ""
        else if str.[0] = 'A' then
            let remaining = str.[1..]
            true, remaining
        else
            false, str

    let testCases = [
    // Test Name           str    Expected Result
        ("A",              "A",   (true, ""))
        ("ABC",            "ABC", (true, "BC"))
        ("<empty string>", "",    (false, ""))
        ("abc",            "abc", (false, "abc"))
        ("xyz",            "xyz", (false, "xyz"))
    ]

module Section_2 = 
    // -----------------------------------------------------------------------
    // Parsing a specified character
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/#implementation-2-parsing-a-specified-character
    // -----------------------------------------------------------------------

    // val pchar : char -> string -> string * string
    let pchar charToMatch str =
        if String.IsNullOrEmpty(str) then
            "No more input", ""
        else
            let first = str.[0]
            if (first = charToMatch) then 
                let remaining = str.[1..]
                let msg = sprintf "Found %c" charToMatch
                msg, remaining
            else
                let msg = sprintf "Expecting %c but found %c" charToMatch first
                msg, str

    let testCases = [
    //   Test Name    charToMatch  str    Expected result
        ("parse 'A' \"A\"",   'A', "A",   ("Found A", ""))
        ("parse 'A' \"ABC\"", 'A', "ABC", ("Found A", "BC"))
        ("parse 'a' \"A\"",   'a', "A",   ("Expecting a but found A", "A"))
        ("parse 'a' \"\"",    'a', "",    ("No more input", ""))
        ]

module Section_3 = 
    // -----------------------------------------------------------------------
    // Returning a Success/Failure
    // -----------------------------------------------------------------------
    // val pchar : char -> string -> Result<char * string>
    let pchar charToMatch str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if (first = charToMatch) then 
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Expecting %c but found %c" charToMatch first
                Failure msg

    let testCases = [
    //   Test Name    charToMatch  str    Expected result
        ("pchar 'A' \"A\"",   'A', "A",   Success ('A', ""))
        ("pchar 'A' \"ABC\"", 'A', "ABC", Success ('A', "BC"))
        ("pchar 'a' \"A\"",   'a', "A",   Failure "Expecting a but found A")
        ("pchar 'a' \"\"",    'a', "",    Failure "No more input")
        ]

 module Section_4 = 
    // -----------------------------------------------------------------------
    // Currying
    // -----------------------------------------------------------------------
    // val pchar : charToMatch:char -> (string -> Result<char * string>)
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
        innerFn

module Section_5 =
    // -----------------------------------------------------------------------
    // Encapsulating the parsing function in a type
    // -----------------------------------------------------------------------
    type Parser<'T> = Parser of (string -> Result<'T * string>)

    // val pchar: char -> Parser<char>
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

    // val run: Parser<'a> -> string -> Result<'a * string>
    let run parser input = 
        // unwrwap the parser to get to the inner function
        let (Parser innerFn) = parser
        innerFn input

    let parse_A = pchar 'A'
    let parse_a = pchar 'a'
    
    let foo = run (pchar 'z') "zee"
    let testCases = [
    //   Test Name            Parser   str    Expected result
        ("run (pchar 'A') \"A\"",   parse_A, "A",   Success ('A', ""))
        ("run (pchar 'A') \"ABC\"", parse_A, "ABC", Success ('A', "BC"))
        ("run (pchar 'a') \"A\"",   parse_a, "A",   Failure "Expecting a but found A")
        ("run (pchar 'a') \"\"",    parse_a, "",    Failure "No more input")
        ]

module Section_6 = 
    // =============================================
    // Section 6 - Combining two parsers in sequence: the "and then" combinator
    // =============================================
    open Section_5

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
    
    let ( .>>. ) = andThen

    // val parseA: Parser<char>
    let parseA = pchar 'A'
    // val parseB: Parser<char>
    let parseB = pchar 'B'

    // val parseAB: Parser<char * char>
    let parseAB = parseA .>>. parseB

    let testCases = [
    //   Test Name               Parser   str     Expected result
        ("run parseAB \"AB\"",   parseAB, "AB",   Success (('A', 'B'), ""))
        ("run parseAB \"ABC\"",  parseAB, "ABC",  Success (('A', 'B'), "C"))
        ("run parseAB \"AC\"",   parseAB, "AC",   Failure "Expecting B but found C")
        ("run parseAB \"A\"",    parseAB, "A",    Failure "No more input")
        ("run parseAB \"\"",     parseAB, "",     Failure "No more input")
    ]

    let foo = run parseAB "AB"

module Section_7 = 
    // =============================================
    // Section 7 - Choosing between two parsers: the “or else” combinator
    // =============================================
    open Section_5

    let orElse parser1 parser2 = 
        let innerFn str =
            let result1 = run parser1 str
            match result1 with
            | Success _ -> 
                result1
            | Failure _ -> 
                let result2 = run parser2 str
                result2
        Parser innerFn

    let ( <!> ) = orElse
        
    // val parseA: Parser<char>
    let parseA = pchar 'A'
    // val parseB: Parser<char>
    let parseB = pchar 'B'

    // val parseAB: Parser<char>
    let parseAB = parseA <!> parseB

    let testCases = [
    //   Test Name                     Parser   str     Expected result
        ("run parse A <!> B \"AB\"",   parseAB, "A",    Success ('A', ""))
        ("run parse A <!> B \"AB\"",   parseAB, "AB",   Success ('A', "B"))
        ("run parse A <!> B \"ABC\"",  parseAB, "BC",   Success ('B', "C"))
        ("run parse A <!> B \"AC\"",   parseAB, "CD",   Failure "Expecting B but found C")
        ("run parse A <!> B \"\"",     parseAB, "",     Failure "No more input")
    ]

module Section_8 =
    // =============================================
    // Section 8 - Combining Parsers
    // =============================================
    open Section_5
    open Section_6
    open Section_7

    let parseC = pchar 'C'

    let borElseC = parseB <!> parseC
    let andThenBorC = parseA .>>. borElseC

    let testCases = [
    //   Test Name                  Parser        str     Expected result
        ("run andThenBorC \"ABZ\"", andThenBorC, "ABZ",   Success (('A', 'B'), "Z"))
        ("run andThenBorC \"ACZ\"", andThenBorC, "ACZ",   Success (('A', 'C'), "Z"))
        ("run andThenBorC \"AC\"",  andThenBorC, "AC",    Success (('A', 'C'), ""))
        ("run andThenBorC \"QBZ\"", andThenBorC, "QBZ",   Failure "Expecting A but found Q")
        ("run andThenBorC \"AQZ\"", andThenBorC, "AQZ",   Failure "Expecting C but found Q")
        ("run andThenBorC \"\"",    andThenBorC,  "",     Failure "No more input")
    ]

module Tests = 
    // -----------------------------------------------------------------------
    // Run The Tests
    // -----------------------------------------------------------------------
    let tests = Seq.concat [ (mapTests1 Section_1.A_Parser id Section_1.testCases) 
                             (mapTests2 Section_2.pchar    id Section_2.testCases)
                             (mapTests2 Section_3.pchar    id Section_3.testCases)
                             (mapTests2 Section_4.pchar    id Section_3.testCases)
                             (mapTests2 Section_5.run      id Section_5.testCases)
                             (mapTests2 Section_5.run      id Section_6.testCases)
                             (mapTests2 Section_5.run      id Section_7.testCases)
                             (mapTests2 Section_5.run      id Section_8.testCases)
                            ]

    [<Tests>] let parserTests = testList "Char Parser Tests" tests