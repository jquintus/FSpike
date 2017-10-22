module NumberParsers

open ParsingTestingUtils
open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote
open FParsec

// Helpers
let identity x = x

// Simple parsers
let float_ws = pfloat .>> ws

// Complex parsers
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"
let intAfterGreaterThans = str ">" >>. str ">" >>. pint32
let manyFloats = many floatBetweenBrackets
let manyFloatsSeperatedByCommas = many (str ">") >>. sepBy floatBetweenBrackets (str ", ")
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

// -----------------------------------------------------------------------
let pfloatTests = [
        ("int parsed as float succeeds",      "1",           Some 1.0 )
        ("non number parsed as float fails",  "Hello world", None )
        ("float parsed as float succeeds",    "1.24",        Some 1.24) ]

[<Tests>]
let pfloatTests' = 
    testList "pfloat Tests" (mapTests1 (getSuccessResult pfloat) id pfloatTests)

[<Tests>]
let parserTests' = 
    testList "Parser Float With Brackets" [
        t "Float between brackets succeeds" <|
            fun _ -> 
                test <@ getSuccessResult floatBetweenBrackets "[1.42]" = Some 1.42 @>
        t "Float without brackets fails" <|
            fun _ -> 
                test <@ getSuccessResult floatBetweenBrackets "1.42" = None @>
        t "Many Floats within bracketss succeed" <|
            fun _ -> 
                test <@ getSuccessResult manyFloats "[1.42][3.14]" = Some [ 1.42; 3.14 ] @>
                        
        t "Empty string returns empty list" <|
            fun _ -> 
                test <@ getSuccessResult manyFloats "" = Some [ ] @>

        t "Comma Sepperated Floats succeed" <|
            fun _ -> 
                test <@ getSuccessResult manyFloatsSeperatedByCommas  ">>>[1.42], [3.14]" = Some [ 1.42; 3.14 ]  @>
        t "Float with trailing white space succeeds" <|
            fun _ ->
                test <@ getSuccessResult float_ws "4.56 " = Some 4.56 @>
        t "Float with leading white space fails" <|
            fun _ ->
                test <@ getSuccessResult float_ws " 4.56" = None @>
        t "Float with no white space succeeds" <|
            fun _ ->
                test <@ getSuccessResult float_ws "4.56" = Some 4.56 @>
        t "Float with multiple trailing white space fails" <|
            fun _ ->
                test <@ getSuccessResult float_ws "4.56  " = Some 4.56 @>
        t "Bracketted float list parses into a list of floats" <|
            fun _ ->
                test <@ getSuccessResult numberList "[      \t 1,     \r\n 4.5   ]  " = Some [ 1.0; 4.5 ] @>
    ]

[<Tests>]
let parserTests'' = 
    testList ">> Int Parser" [
        t ">> 45 returns 45" <|
            fun _ -> 
                test <@ getSuccessResult intAfterGreaterThans ">>45" = Some 45 @>
        t "Doing math in the parser" <|
            fun _ ->
                let parser = pipe3 pint32 (ws_str_ws "+") pint32 (fun a _ b -> a + b)
                test <@ getSuccessResult parser "2 + 3" = Some 5 @>
    ]
