// reference:  http://www.quanttec.com/fparsec/tutorial.html#parsing-a-float-between-brackets
module StringParsers

open ParsingTestingUtils
open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote
open FParsec

// Simple parsers
let str s = pstring s
let ws = spaces

// Parsers
let stringLiteral s = 
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    let quoteParser = (pstring "\"")
    let stringLiteralParser = manyChars (normalChar <|> escapedChar)
    between quoteParser quoteParser stringLiteralParser s

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

        t "String literal" <|
            fun _ ->
                test <@ getSuccessResult stringLiteral "\"hello world\"" = Some "hello world" @>
        t "String literal missing closing quote" <|
            fun _ ->
                test <@ getSuccessResult stringLiteral "\"hello world" = None @>
    ]
