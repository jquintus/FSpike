module SimpleParser

open Fuchu
open FParsec
open Swensen.Unquote.Assertions
open Swensen.Unquote

let startsWith input (s:string) = s.StartsWith(input)

let testParser' p str = 
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Error: %A" errorMsg

let testParser p str = 
    match run p str with
    | Success(result, _, _)   -> Some result
    | Failure(_, _, _)        -> None

[<Tests>]
let parserTests = 
    testList "Parser Tests" [
        testCase "int parsed as float succeeds" <|
            fun _ ->
                testParser pfloat "1" =! Some 1.0
                ()

        testCase "non number parsed as float fails" <|
            fun _ ->
                testParser pfloat "hello world" =! None
                ()

        testCase "Float parsed as float succeeds" <|
            fun _ ->
                test <@ testParser pfloat "1.24" = Some 1.24 @>

    ]