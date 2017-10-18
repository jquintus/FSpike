module SimpleTests

open Fuchu

let alwaysTrue x = true
let head array = 
    array |> Array.find alwaysTrue

[<Tests>]
let tests = 
    testList "A test group" [
        testCase "one test" <|
            fun _ -> Assert.Equal("2+2", 4, 2+2)
        testCase "another test" <|
            fun _ -> Assert.Equal("3+3", 3, 3+3)
    ]

[<Tests>]
let tests2 =
    testList "My Simple Tests" [
        testCase "Arrays" <|
            fun _ -> 
                let myHead = [| 1; 2; 3 |] |> head
                Assert.Equal("head of [| 1; 2; 3 |]", 1, myHead)

    ]