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

let MapJObject value = JObject (Map value)


let testInput = [
    ("True Parses",    @"{ ""Key"" : true }",        MapJObject [("Key", JBool(true))] )
    ("False Parses",   @"{ ""Key"" : false }",       MapJObject [("Key", JBool(false))] )
                                                     
    ("Nulls Parse",    @"{ ""Key"" : null }",        MapJObject [("Key", JNull)] )
                                                     
    ("ints Parse",     @"{ ""Key"" : 42 }",          MapJObject [("Key", JNumber (42.0))] )
    ("floats Parse",   @"{ ""Key"" : 4.2 }",         MapJObject [("Key", JNumber (4.2))] )

    ("Strings Parse",  @"{ ""Key"" : ""hello"" }",   MapJObject [("Key", JString("hello"))] )

    ("MultipleValues Parse",  
                       @"{ ""Key1"" : ""hello"",
                           ""Key2"" : ""World""
                       }",   
                                                     MapJObject [
                                                        ("Key1", JString("hello"))
                                                        ("Key2", JString("World"))
                                                        ] )
    ("Lists Parse",
                        @"{ ""bools"" : [true,  true, false] }",
                                                     MapJObject [
                                                        ("bools", JList ( [ 
                                                                            JBool(true)
                                                                            JBool(true)
                                                                            JBool(false) ]))
                                                        ] )
]

[<Tests>]
let parserTests' = 
    testList "JSON Parser" ( mapJsonTests testInput )