module SimpleParser

open Fuchu
open FParsec
open Swensen.Unquote.Assertions
open Swensen.Unquote





let str s =pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"
let intAfterGreaterThans = str ">" >>. str ">" >>. pint32
let manyFloats = many floatBetweenBrackets
let manyFloatsSeperatedByCommas = many (str ">") >>. sepBy floatBetweenBrackets (str ", ")

let startsWith input (s:string) = s.StartsWith(input)

let testParser' p str = 
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Error: %A" errorMsg

let getSuccessResult p str = 
    match run p str with
    | Success(result, _, _)   -> Some result
    | Failure(_, _, _)        -> None
let getFailureMessage p str =
    match run p str with
    | Success(_, _, _)   -> None
    | Failure(errorMsg, _, _) -> Some errorMsg
    

[<Tests>]
let parserTests = 
    testList "Parser Tests" [
        testCase "int parsed as float succeeds" <|
            fun _ ->
                getSuccessResult pfloat "1" =! Some 1.0
                ()

        testCase "non number parsed as float fails" <|
            fun _ ->
                getSuccessResult pfloat "hello world" =! None
                ()

        testCase "Float parsed as float succeeds" <|
            fun _ ->
                test <@ getSuccessResult pfloat "1.24" = Some 1.24 @>
    ]

[<Tests>]
let parserTests' = 
    testList "Parser Float With Brackets" [
        testCase "Float between brackets succeeds" <|
            fun _ -> 
                test <@ getSuccessResult floatBetweenBrackets "[1.42]" = Some 1.42 @>
        testCase "Float without brackets fails" <|
            fun _ -> 
                test <@ getSuccessResult floatBetweenBrackets "1.42" = None @>
        testCase "Many Floats within bracketss succeed" <|
            fun _ -> 
                test <@ getSuccessResult manyFloats "[1.42][3.14]" = Some [ 1.42; 3.14 ] @>
                        
        testCase "Empty string returns empty list" <|
            fun _ -> 
                test <@ getSuccessResult manyFloats "" = Some [ ] @>

        testCase "Comma Sepperated Floats succeed" <|
            fun _ -> 
                test <@ getSuccessResult manyFloatsSeperatedByCommas  ">>>[1.42], [3.14]" = Some [ 1.42; 3.14 ]  @>
    ]

[<Tests>]
let parserTests'' = 
    testList ">> Int Parser" [
        testCase ">> 45 returns 45" <|
            fun _ -> 
                test <@ getSuccessResult intAfterGreaterThans ">>45" = Some 45 @>
    ]