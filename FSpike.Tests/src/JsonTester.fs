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


let mapJsonTests = mapTests1 (getSuccessResult json) Some

let testInput = [
    ("True Parses",    @"{ ""Key"" : true }",        JObject (Map [("Key", JBool(true))]) )
    ("False Parses",   @"{ ""Key"" : false }",       JObject (Map [("Key", JBool(false))]) )
                                                     
    ("Null Parses",    @"{ ""Key"" : null }",        JObject (Map [("Key", JNull)]) )
                                                     
    ("int Parses",     @"{ ""Key"" : 42 }",          JObject (Map [("Key", JNumber (42.0))]) )
    ("float Parses",   @"{ ""Key"" : 4.2 }",         JObject (Map [("Key", JNumber (4.2))]) )

    ("String Parses",  @"{ ""Key"" : ""hello"" }",   JObject (Map [("Key", JString("hello"))]) )
]

[<Tests>]
let parserTests' = 
    testList "JSON Parser" ( mapJsonTests testInput )

[<Tests>]
let parserTests = 
    testList "JSON Parser" [

        //t "list parses" <|
        //    fun _ -> 
        //        let input = @"{ ""Key"" : [""A"" : ""B""] }"
        //        let expected = JObject (Map [("Key", JList Map [("A", "B")])])

        //        testJson input expected

]