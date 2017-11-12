module CharParser
open System
open Fuchu

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
    type Result<'a> = 
        | Success of 'a
        | Failure of string

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
    type Result<'a> = 
        | Success of 'a
        | Failure of string

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

    let testCases = [
    //   Test Name    charToMatch  str    Expected result
        ("currying pchar 'A' \"A\"",   'A', "A",   Success ('A', ""))
        ("currying pchar 'A' \"ABC\"", 'A', "ABC", Success ('A', "BC"))
        ("currying pchar 'a' \"A\"",   'a', "A",   Failure "Expecting a but found A")
        ("currying pchar 'a' \"\"",    'a', "",    Failure "No more input")
        ]
module Tests = 
    // -----------------------------------------------------------------------
    // Run The Tests
    // -----------------------------------------------------------------------
    let tests = Seq.concat [ (mapTests1 Section_1.A_Parser id Section_1.testCases) 
                             (mapTests2 Section_2.pchar    id Section_2.testCases)
                             (mapTests2 Section_3.pchar    id Section_3.testCases)
                             (mapTests2 Section_4.pchar    id Section_4.testCases)
                            ]

    [<Tests>] let parserTests = testList "Char Parser Tests" tests