module ParsingTestingUtils

open Fuchu
open FParsec

// Testing 
let startsWith input (s:string) = s.StartsWith(input)
let t = testCase

// Parsing
let str s = pstring s
let ws = spaces
let str_ws s = str s .>> ws
let ws_str_ws s = ws >>. str s .>> ws

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