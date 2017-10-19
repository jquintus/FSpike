﻿module SimpleTests

open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote
open Swensen.Unquote.Operators

let alwaysTrue x = true
let head array = 
    array |> Array.find alwaysTrue

[<Tests>]
let tests = 
    testList "A test group" [
        testCase "one test" <|
            fun _ -> Assert.Equal("2+2", 4, 2+2)
        testCase "another test" <|
            fun _ -> Assert.Equal("3+3", 6, 3+3)
    ]

[<Tests>]
let tests2 =
    testList "Unquote Tests" [
        testCase "Head returns the first item in the array" <|
            fun _ -> 
                test <@ [| 1; 2; 3 |] |> head = 1 @> |> ignore
        testCase "map applies function to every item in a sequence" <|
            fun _ ->
                test <@ ([1; 2; 3; 4] |> List.map ((+) 1)) = [ 2..5 ] @> |> ignore

        testCase "Map using the =! operator prints out less than the <@ .. @>" <|
            fun _ ->
               ([1; 2; 3; 4;] |> List.map ((+) 1)) =! [ 2..5 ]

]