module NumberParsers

open ParsingTestingUtils
open Fuchu
open FParsec

// -----------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------
let float_ws = pfloat .>> ws
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"
let intAfterGreaterThans = str ">" >>. str ">" >>. pint32
let manyFloats = many floatBetweenBrackets
let manyFloatsSeperatedByCommas = many (str ">") >>. sepBy floatBetweenBrackets (str ", ")
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"
let additionParser = pipe3 pint32 (ws_str_ws "+") pint32 (fun a _ b -> a + b)

// -----------------------------------------------------------------------
// Parser test cases
// -----------------------------------------------------------------------
let floatParserTests = [
    (@"pfloat ""1"" succeeds",              pfloat,               "1",                 Some 1.0 )
    (@"pfloat ""Hello world"" fails",       pfloat,               "Hello world",       None )
    (@"pfloat ""1.24"" succeeds",           pfloat,               "1.24",              Some 1.24)
    (@"floatBetweenBrackets succeeds",      floatBetweenBrackets, "[1.42]",            Some 1.42)
    (@"floatBetweenBrackets fails",         floatBetweenBrackets, "1.42",              None)
    (@"Float with trailing white space succeeds",                                      
                                            float_ws,             "4.56 ",             Some 4.56 )
    (@"Float with leading white space fails",                                          
                                            float_ws,             " 4.56",             None )
    (@"Float with no white space succeeds",                                            
                                            float_ws,             "4.56",              Some 4.56 )
    (@"Float with multiple trailing white space fails",                                
                                            float_ws,             " 4.56  ",           None )
]                                                                                      
                                                                                       
let intParserTests = [                                                                 
    (@"int parser "">> 45"" returns 45",    intAfterGreaterThans, ">>45",              Some 45)
    (@"Doing math in the parser",           additionParser,       "42 + 3",            Some 45)
]                                                                                      
                                                                                       
let listParserTests = [                                                                
    (@"Bracketted float list parses into a list of floats",                            
                                            numberList,           "[  \t 1,  \r\n 4.5   ]  ",     
                                                                                       Some [ 1.0; 4.5 ] )
    (@"Many Floats within bracketss succeed",                                          
                                            manyFloats,           "[1.42][3.14]",      Some [ 1.42; 3.14 ] )
    (@"Empty string returns empty list",    manyFloats,           "",                  Some [ ] )
    (@"Comma Sepperated Floats succeed",    manyFloatsSeperatedByCommas,  
                                                                  ">>>[1.42], [3.14]", Some [ 1.42; 3.14 ] )
]

// -----------------------------------------------------------------------
// Run The Tests
// -----------------------------------------------------------------------
let tests = Seq.concat [ (mapTests2 getSuccessResult id floatParserTests) 
                         (mapTests2 getSuccessResult id intParserTests)
                         (mapTests2 getSuccessResult id listParserTests)
                        ]

[<Tests>] let parserTests = testList "Parser Tests" tests