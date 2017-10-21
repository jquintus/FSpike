module StringParsers

open ParsingTestingUtils
open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote
open FParsec

// Simple parsers
let str s = pstring s
let ws = spaces

[<Tests>]
let parserTests = 
    testList "String Parser" [
        t "???" <|
            fun _ -> 
                test <@ 34 = 34 @>
    ]
