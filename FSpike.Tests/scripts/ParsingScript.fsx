// Note: FParsecCS needs to listed FIRST
#r @"..\..\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"

#load @"..\src\JsonParser.fs"

open FParsec 
open JsonParser

let bad = run pfloat "landed"
let good = run pfloat "3.14"


run json @"{ ""key"" : [ ]  }"

