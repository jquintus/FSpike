module ParsingTestingUtils

open Fuchu
open FParsec
open Swensen.Unquote.Assertions
open Swensen.Unquote

// Testing 
let startsWith input (s:string) = s.StartsWith(input)
let t = testCase

let mapTests2 cases f =
    cases |> Seq.map (fun case -> 
        let (testName, param1, param2, expected) = case
        testCase testName <|
            fun _ ->
                test <@ f param1 param2 = expected @>
        )

let mapTests1 fLeft fRight cases =
    cases |> Seq.map (fun case -> 
        let (testName, input, expected) = case
        testCase testName <|
            fun _ ->
                test <@ fLeft input = fRight expected @>
        )

// Parsing
let str s = pstring s
let ws = spaces
let str_ws s = str s .>> ws
let ws_str_ws s = ws >>. str s .>> ws

let testParser p str = 
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Error: %A" errorMsg

let result2Str result =
    match result with
    | Success(result, _, _)   -> sprintf "%A" result
    | Failure(errorMsg, _, _) -> sprintf "%A" errorMsg

let getSuccessResult p str = 
    match run p str with
    | Success(result, _, _)   -> Some result
    | Failure(_, _, _)        -> None
let getFailureMessage p str =
    match run p str with
    | Success(_, _, _)   -> None
    | Failure(errorMsg, _, _) -> Some errorMsg