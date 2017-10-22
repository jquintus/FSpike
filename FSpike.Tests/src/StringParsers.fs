// reference:  http://www.quanttec.com/fparsec/tutorial.html#parsing-a-float-between-brackets
module StringParsers

open ParsingTestingUtils
open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote
open FParsec
open NumberParsers

// Types
type StringConstant = StringConstant of string * string

// Simple parsers
let str s = pstring s
let ws = spaces

// Parsers
let quoteParser = (pstring "\"")
let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                        | 'n' -> "\n"
                                                        | 'r' -> "\r"
                                                        | 't' -> "\t"
                                                        | c   -> string c)

let stringLiteral1 s =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    let stringLiteralParser = manyChars (normalChar <|> escapedChar)
    between quoteParser quoteParser stringLiteralParser s


let stringLiteral2 = 
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let stringLiteralParser = manyStrings (normalCharSnippet <|> escapedChar)
    between quoteParser quoteParser stringLiteralParser

let stringLiteral3 =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let stringLiteralParser = stringsSepBy normalCharSnippet escapedChar
    between quoteParser quoteParser stringLiteralParser

let identifier' = 
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws 

[<Tests>]
let parserTests = 
    testList "String Parser" [
        t "Many orrred together returns list of each item" <|
            fun _ -> 
                // Assemble
                let pA = str "a" <|> str "A"
                let pB = str "b" <|> str "B"
                let parser = many ( pA <|> pB)

                let expected = [ "a"; "a"; "A"; "B"; "b"; "A"; "a"  ]

                test <@ getSuccessResult parser "aaABbAa" = Some expected @>

        t "Skip string" <|
            fun _ ->
                let parser = skipStringCI "hello " >>. pstringCI "world"
                test <@ getSuccessResult parser "HeLlO WORLD" = Some "WORLD" @>

        t "Skip string repeated" <|
            fun _ -> 
                // Assemble
                let pA = str "a" <|> str "A"
                let pB = skipStringCI "b"
                let parser = many ( pB >>. pA)

                let expected = [ "a"; "A";  ]

                test <@ getSuccessResult parser "baBA" = Some expected @>

        t "Filter out Bs" <|
            fun _ -> 
                // Assemble
                let pA = str "a" <|> str "A"
                let toEmpty b = ""
                let pB = anyOf "bB" |>> toEmpty
                let parser = many ( pB <|> pA)

                // Still not the filter I'm looking for.  How do you skip a letter repeatedly?
                let expected = [""; "a"; ""; ""; ""; "a"; "A"]

                test <@ getSuccessResult parser "baBbbaA" = Some expected @>

        t "String literal" <|
            fun _ ->
                test <@ getSuccessResult stringLiteral1 "\"hello world\"" = Some "hello world" @>
                test <@ getSuccessResult stringLiteral2 "\"hello world\"" = Some "hello world" @>
                test <@ getSuccessResult stringLiteral3 "\"hello world\"" = Some "hello world" @>
        t "String literal missing closing quote" <|
            fun _ ->
                test <@ getSuccessResult stringLiteral1 "\"hello world" = None @>
                test <@ getSuccessResult stringLiteral2 "\"hello world" = None @>
                test <@ getSuccessResult stringLiteral3 "\"hello world" = None @>

        t "StringsSepBy returns all the strings" <|
            fun _ ->
                let aParser = (many1Satisfy (fun c -> c = 'a'))
                let sepParser = (many1Satisfy (fun c -> c = 'b'))
                let parser = stringsSepBy aParser sepParser

                test <@ getSuccessResult parser "aabaa" = Some "aabaa" @>
        t "Piping it all together" <|
            fun _ -> 
                let joiner id _ str = StringConstant (id, str)
                let expected = StringConstant ("myString", "myValue")
                let parser = pipe3 identifier' (str_ws "=") stringLiteral3  joiner
                test <@ getSuccessResult parser "myString = \"myValue\"" = Some expected @>
        t "Tupling it all together" <|
            fun _ -> 
                // note: Like above, but using the built in parser to return a tuple
                //       We can't exclude the middle parameter if we use the built in parser
                let expected = "myString", "=", "myValue"
                let parser = tuple3 identifier' (str_ws "=") stringLiteral3
                test <@ getSuccessResult parser "myString = \"myValue\"" = Some expected @>
    ]