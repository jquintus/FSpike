module NameParser
open NameParserModels

open FParsec

let upperChars = satisfy (fun c-> isLetter c && isUpper c)
let lowerChars = satisfy (fun c-> isLetter c && isLower c)

let concatName (upper:char) (lowers:char list) = 
    upper :: lowers 
    |> Array.ofList 
    |> System.String.Concat

let pname  = pipe2 upperChars (many lowerChars) concatName

let ws = spaces

let triNameParser = pipe3 (pname .>> ws) (pname .>> ws) (pname) (fun f m l -> createName3 f m l)
let duoNameParser = pipe2 (pname .>> ws) pname (fun f l -> createName2 f l)
let allNameParser = choice [ duoNameParser; triNameParser  ] 

module Tests =
    open ParsingTestingUtils
    open Fuchu
    open Swensen.Unquote
    open Swensen.Unquote.Assertions
    [<Tests>]
    let tests = 
        testList "Name Parser Tests" [
            testCase "triNameParser on 'Josh Paul Quintus' succeeds" <|
                fun _ -> 
                  test <@ getSuccessResult triNameParser "Josh Paul Quintus" = (createName3 "Josh" "Paul" "Quintus" |> Some) @>
            testCase "triNameParser on 'Ksenia Winnicki' fails" <|
                fun _ -> 
                  test <@ getSuccessResult triNameParser "Ksenia Winnicki" = None @>
            testCase "duoNameParser on 'Ksenia Winnicki' succeeds" <|
                fun _ -> 
                  test <@ getSuccessResult duoNameParser "Ksenia Winnicki" = (createName2 "Ksenia" "Winnicki" |> Some) @>

            // =========================================
            testCase "allNameParser on 'Josh Paul Quintus' succeeds" <|
                fun _ -> 
                  test <@ getSuccessResult allNameParser "Josh Paul Quintus" = (createName3 "Josh" "Paul" "Quintus" |> Some) @>
            testCase "allNameParser on 'Ksenia Winnicki' succeeds" <|
                fun _ -> 
                  test <@ getSuccessResult allNameParser "Ksenia Winnicki" = (createName2 "Ksenia" "Winnicki" |> Some) @>
        ]