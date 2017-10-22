module JsonTester

open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote
open ParsingTestingUtils
open JsonParser
open FParsec

let testJson input expected =
    test <@ getSuccessResult json input = Some expected @>

let testJson' input =
    let result = run json input |> result2Str
    test <@ result = "dummy" @>


[<Tests>]
let parserTests = 
    testList "JSON Parser" [
        t "True parses" <|
            fun _ -> 
                let input = @"{ ""Key"" : true }"
                let expected = JObject (Map [("Key", JBool(true))])

                testJson input expected

        t "False parses" <|
            fun _ -> 
                let input = @"{ ""Key"" : false }"
                let expected = JObject (Map [("Key", JBool(false))])

                testJson input expected

        t "int parses" <|
            fun _ -> 
                let input = @"{ ""Key"" : 42 }"
                let expected = JObject (Map [("Key", JNumber(42.0))])

                testJson input expected
        t "float parses" <|
            fun _ -> 
                let input = @"{ ""Key"" : 4.2 }"
                let expected = JObject (Map [("Key", JNumber(4.2))])

                testJson input expected

        t "string parses" <|
            fun _ -> 
                let input = @"{ ""Key"" : ""hello"" }"
                let expected = JObject (Map [("Key", JString("hello"))])

                testJson input expected

        t "null parses" <|
            fun _ -> 
                let input = @"{ ""Key"" : null }"
                let expected = JObject (Map [("Key", JNull)])

                testJson input expected

        //t "list parses" <|
        //    fun _ -> 
        //        let input = @"{ ""Key"" : [""A"" : ""B""] }"
        //        let expected = JObject (Map [("Key", JList Map [("A", "B")])])

        //        testJson input expected

]