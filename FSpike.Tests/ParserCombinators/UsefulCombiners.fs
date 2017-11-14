module UsefulCombiners
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/
open System
open Fuchu
open Swensen.Unquote.Assertions
open Swensen.Unquote

module StartingPoint =
    /// Type that represents Success/Failure in parsing
    type Result<'a> =
        | Success of 'a
        | Failure of string

    /// Type that wraps a parsing function
    type Parser<'T> = Parser of (string -> Result<'T * string>)

    /// Parse a single character
    let pchar charToMatch =
        // define a nested inner function
        let innerFn str =
            if String.IsNullOrEmpty(str) then
                Failure "No more input"
            else
                let first = str.[0]
                if first = charToMatch then
                    let remaining = str.[1..]
                    Success (charToMatch,remaining)
                else
                    let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                    Failure msg
        // return the "wrapped" inner function
        Parser innerFn

    /// Run a parser with some input
    let run parser input =
        // unwrap parser to get inner function
        let (Parser innerFn) = parser
        // call inner function with input
        innerFn input

    /// Combine two parsers as "A andThen B"
    let andThen parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input

            // test the result for Failure/Success
            match result1 with
            | Failure err ->
                // return error from parser1
                Failure err

            | Success (value1,remaining1) ->
                // run parser2 with the remaining input
                let result2 =  run parser2 remaining1

                // test the result for Failure/Success
                match result2 with
                | Failure err ->
                    // return error from parser2
                    Failure err

                | Success (value2,remaining2) ->
                    // combine both values as a pair
                    let newValue = (value1,value2)
                    // return remaining input after parser2
                    Success (newValue,remaining2)

        // return the inner function
        Parser innerFn

    /// Infix version of andThen
    let ( .>>. ) = andThen

    /// Combine two parsers as "A orElse B"
    let orElse parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input

            // test the result for Failure/Success
            match result1 with
            | Success result ->
                // if success, return the original result
                result1

            | Failure err ->
                // if failed, run parser2 with the input
                let result2 = run parser2 input

                // return parser2's result
                result2

        // return the inner function
        Parser innerFn

    /// Infix version of orElse
    let ( <|> ) = orElse

    /// Choose any of a list of parsers
    let choice listOfParsers =
        List.reduce ( <|> ) listOfParsers

    /// Choose any of a list of characters
    let anyOf listOfChars =
        listOfChars
        |> List.map pchar // convert into parsers
        |> choice

module Section_1  =
    // =============================================
    // Section 1 - Map
    // =============================================
    open StartingPoint

    let parseDigit = anyOf ['0'..'9']

    // val parseThreeDigits: Parser<(char * char) * char>
    let parseThreeDigits =
        parseDigit .>>. parseDigit .>>. parseDigit

    // val mapP: ('a -> 'b) -> Parser<'a> -> Parser<'b>
    let mapP f parser =
        let innerFn input =
            let result = run parser input
            match result with
            | Success (value, remaining) ->
                let newValue = f value
                Success (newValue, remaining)
            | Failure err ->
                Failure err

        Parser innerFn

    let ( <!> ) = mapP
    let ( |>> ) x f = mapP f x

    // val parse3DigitsAsStr: Parse<string>
    let parse3DigitsAsStr =
        let transformTuple ((c1, c2), c3) =
            String [| c1; c2; c3; |]
        mapP transformTuple parseThreeDigits

    let parse3DigitsAsStr' =
        parseThreeDigits
        |>> fun((c1, c2), c3) -> String [| c1; c2; c3; |]

    // val parse3DigitsAsInt: Parse<int>
    let parse3DigitsAsInt =
        parse3DigitsAsStr
        |>> fun str -> int str

    // ----------------------------------------------------------------
    let run = StartingPoint.run
    let testCases  = 
        [
            testCase "parseThreeDigits 911A" <| fun _ ->
                test <@ run parseThreeDigits  "911A" = Success ((('9', '1'), '1'), "A") @>
            testCase "parse3DigitsAsStr 911A" <| fun _ ->
                test <@ run parse3DigitsAsStr "911A" = Success ("911", "A") @>
            testCase "parse3DigitsAsStr' 911A" <| fun _ ->
                test <@ run parse3DigitsAsStr' "911A" =  Success ("911", "A") @>
            testCase "parse3DigitsAsInt 911A" <| fun _ -> 
                test <@ run parse3DigitsAsInt "911A" = Success (911, "A") @>
        ]

module Section_2  =
    // =============================================
    // Section 2 - apply and return -> lifting functions to the Parser world
    // =============================================
    open StartingPoint
    open Section_1

    // val returnP: 'a -> Parser<'a>
    let returnP x =
        let innerFn input =
            Success (x, input)
        Parser innerFn
    
    // val Parser<( 'a -> 'b ) -> Parser<'a> -> Parser<'b>
    let applyP fP xP =
        // create a parser containing a pair (f,x)
        // and then map the pair by applying f to x
        (fP .>>. xP)
        |> mapP (fun (f, x) -> f x)

    let ( <*> ) = applyP

    // val lift2: f:('a -> 'b -> 'c) -> Parser<'a> -> Parser<'b> -> Parser<'c'>
    let lift2 f xP yP = 
        returnP f <*> xP <*> yP
    
    let lift3 f xP yP zP =
        returnP f <*> xP <*> yP <*> zP
    
    // ----------------------------------------------------------------

    // val addP -> Parser<int> -> Parser<int> -> Parser<int>
    let addP = lift2 (+) 

    // val startsWith: string -> string -> bool
    let startsWith (str:string) prefix = 
        str.StartsWith(prefix)
    
    // val startsWithP: Parser<string> -> Parser<string> -> Parser<bool>
    let startsWithP = lift2 startsWith 

    // ----------------------------------------------------------------
    let parseDigitAsInt= anyOf ['0'..'9'] |>> fun char -> int char - int '0'

    let run = StartingPoint.run
    let testCases  = 
        [
            testCase "returnP 5" <| fun _ ->
                let fiveParser = returnP 5
                test <@ run fiveParser "anything" = Success (5, "anything") @>

            testCase "returnP 'Hello'" <| fun _ ->
                let helloParser = returnP "hello"
                test <@ run helloParser "xyz" = Success ("hello", "xyz") @>

            testCase "addP '2345' " <| fun _ ->
                let addFirstTwoInts = addP parseDigitAsInt parseDigitAsInt
                test <@ run addFirstTwoInts "2345" = Success(5, "45") @>
            
            testCase "lift3 - triple adds" <| fun _ ->
                let add3P = lift3 (fun x y z -> x + y + z)
                let addFirstThreeInts = add3P parseDigitAsInt parseDigitAsInt parseDigitAsInt
                test <@ run addFirstThreeInts "2345" = Success(9, "5") @>
        ]

module Section_3  =
    // =============================================
    // Section 3 - Lists of parsers into a single Parser
    // =============================================
    open StartingPoint
    open Section_1
    open Section_2

    // val sequence Parser<'a> list -> Parser<'a list>
    let rec sequence parserList = 
        let cons head tail = head::tail
        let consP = lift2 cons

        match parserList with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

    // val charListToStr: char list -> string
    let charListToStr charList = String(List.toArray charList)

    // val pstring: string -> Parser<string>
    let pstring str = 
        str
        |> List.ofSeq          // convert to List of char
        |> List.map pchar      // map each char to a parser
        |> sequence            // convert to a single Parser<char list>
        |> mapP charListToStr  // convert Parser<char list> to Parser<string>

    let parseABC = pstring "ABC"
    // ----------------------------------------------------------------
    let run = StartingPoint.run
    let testCases  = 
        [
            testCase "run combined ABCDE" <| fun _ ->
                let parsers = [pchar 'A'; pchar 'B'; pchar 'C']
                let combined = sequence parsers
                test <@ run combined "ABCDE" = Success (['A'; 'B'; 'C'], "DE") @>
            testCase "run parseABC ABC" <| fun _ ->
                test <@ run parseABC "ABC" =  Success ("ABC", "")@>
            testCase "run parseABC ABCDE" <| fun _ ->
                test <@ run parseABC "ABCDE" =  Success ("ABC", "DE")@>
            testCase "run parseABC AB|C" <| fun _ ->
                test <@ run parseABC "AB|C" =  Failure "Expecting 'C'. Got '|'" @>
       ]

 module Section_4  =
    // =============================================
    // Section 4 - many and many1
    // =============================================
    open StartingPoint
    open Section_1
    open Section_2
    open Section_3

    // val parseZeroOrMore: Parser<'a> -> 'a list * string
    let rec parseZeroOrMore parser input = 
        let firstResult = run parser input
        match firstResult with
        | Failure _ ->
            ([], input)
        | Success (firstValue, inputAfterFirstParse) -> 
            let (subsequentValues, remainingInput) = 
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue::subsequentValues
            (values, remainingInput)
    
    // val many: Parser<'a> -> Parser<'a list>
    let many parser =
        let innerFn input =
            Success (parseZeroOrMore parser input)
        Parser innerFn
    
    // val many1: Parser<'a> -> Parser<'a list>
    let many1 parser =
        let innerFn input =
            let firstResult = run parser input
            match firstResult with
            | Failure err -> 
                Failure err
            | Success (firstValue, inputAfterFirstParse) -> 
                let (subsequentValues, remainingInput) = 
                    parseZeroOrMore parser inputAfterFirstParse
                let values = firstValue::subsequentValues
                Success (values, remainingInput)
        Parser innerFn

    // ----------------------------------------------------------------
    let run = StartingPoint.run

    let manyA = many (pchar 'A')
    let manyAB = many (pstring "AB")
    let ws = many (anyOf [' '; '\t'; '\n'])
    let digits = many1 (anyOf ['0'..'9'])
    let pint = 
        let resultToInt digitList = 
            String(List.toArray digitList) |> int
        digits |>> resultToInt

    let testCases = [
    //   Test Name           Parser  str      Expected result
        ("run manyA ABC",    manyA,  "ABC",    Success (['A'], "BC"))
        ("run manyA AAC",    manyA,  "AAC",    Success (['A'; 'A'], "C"))
        ("run manyA AAA",    manyA,  "AAA",    Success (['A'; 'A'; 'A'], ""))
        ("run manyA BBC",    manyA,  "BBC",    Success ([], "BBC"))
        ("run manyA ''" ,    manyA,  ""   ,    Success ([], ""))
        ("run ws 'ABC'",     ws,     "ABC",    Success ([], "ABC"))
        ("run ws ' ABC'",    ws,     " ABC",   Success ([' '], "ABC"))
        ("run ws '\\t AB'",  ws,     "\t AB",  Success (['\t'; ' '], "AB"))
        ("run digits 123",   digits, "123",    Success (['1'; '2'; '3'], ""))
        ("run digits 1AB",   digits, "1AB",    Success (['1'], "AB"))
        ("run digits AB",    digits, "AB",     Failure ("Expecting '9'. Got 'A'"))
    ]

    let pintTestCases = [
        ("run pint 123", pint, "123",  Success(123, ""))
        ("run pint 12A", pint, "12A",  Success(12, "A"))
        ("run pint ''",  pint, "",     Failure("No more input"))
        ("run pint ABC", pint, "ABC",  Failure ("Expecting '9'. Got 'A'"))
    ]

 module Section_5  =
    // =============================================
    // Section 5 - opt - zero or one match
    // =============================================
    open StartingPoint
    open Section_1
    open Section_2
    open Section_3
    open Section_4

    let opt p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let digit = anyOf ['0'..'9']

    // val digitThenSemicolon : Parser<char * char option>
    let digitThenSemicolon = digit .>>. opt (pchar ';')

    let pint = 
        let resultToInt (sign, digitList) =
            let i = String(List.toArray digitList) |> int
            match sign with
            | Some _ -> -i
            | None   ->  i

        opt (pchar '-') .>>. digits
        |>> resultToInt
    
    // ----------------------------------------------------------------
    let run = StartingPoint.run
    let testCases  = 
        [
            testCase "digitThenSemicolon 1;" <| fun _ ->
                test <@ run digitThenSemicolon "1;" = Success (('1', Some ';'), "") @>
            testCase "digitThenSemicolon 1" <| fun _ ->
                test <@ run digitThenSemicolon "1" = Success (('1', None), "") @>

            testCase "pint 123" <| fun _ ->
                test <@ run pint "123" = Success(123, "") @>
            testCase "pint 123ABC" <| fun _ ->
                test <@ run pint "123ABC" = Success(123, "ABC") @>
            testCase "pint 0" <| fun _ ->
                test <@ run pint "0" = Success(0, "") @>

            // Negative cases
            testCase "pint 123" <| fun _ ->
                test <@ run pint "123" = Success(123, "") @>
            testCase "pint -123ABC" <| fun _ ->
                test <@ run pint "-123ABC" = Success(-123, "ABC") @>
        ]

 module Section_6  =
    // =============================================
    // Section 6 - Throwing results away
    // =============================================
    open StartingPoint
    open Section_1
    open Section_2
    open Section_3
    open Section_4
    open Section_5

    let (.>>) p1 p2 =
        p1 .>>. p2
        |>> fun (a, _) -> a

    let (>>.) p1 p2 = 
        p1 .>>. p2
        |>> fun (_, b) -> b

    let between p1 p2 p3 = 
        p1 >>. p2 .>> p3

    // ----------------------------------------------------------------
    let run = StartingPoint.run

    let digitThenSemicolon =  digit .>> opt (pchar ';')
    let quote = pchar '"'
    let quotedInt = between quote Section_5.pint quote

    let testCases  = 
        [
            testCase "digitThenSemicolon' 1;" <| fun _ ->
                test <@ run digitThenSemicolon "1;" = Success ('1', "") @>
            testCase "digitThenSemicolon' 1" <| fun _ ->
                test <@ run digitThenSemicolon "1" = Success ('1', "") @>
            testCase "quotedInt \"123\"" <| fun _ ->
                test <@ run quotedInt "\"123\"" = Success(123, "") @>
            testCase "quotedInt ABC" <| fun _ ->
                test <@ run quotedInt "ABC" = Failure "Expecting '\"'. Got 'A'" @>
            testCase "quotedInt \"AB\"" <| fun _ ->
                test <@ run quotedInt "\"ABC\"" = Failure "Expecting '9'. Got 'A'" @>
         ]

 module Section_7  =
    // =============================================
    // Section 7 - Parsing lists with separators
    // =============================================
    open StartingPoint
    open Section_1
    open Section_2
    open Section_3
    open Section_4
    open Section_5
    open Section_6

    // val sepBy1: Parser<'a> -> Parser<'b> -> Parser<'a list>
    let sepBy1 p sep = 
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> fun (p, pList) -> p::pList
    
    // this is my implementation before looking at the solution
    // val sepBy: Parser<'a> -> Parser<'b> -> Parser<'a list>
    let sepBy' p sep = 
        let sepThenP = sep >>. p
        (opt p) .>>. many sepThenP
        |>> fun (p, pList) ->
            match p with
            | Some pValue -> pValue::pList
            | None -> pList
    
    // The more elegant text book answser
    let sepBy p sep = 
        sepBy1 p sep <|> returnP [ ]

    // ----------------------------------------------------------------
    let comma = pchar ','
    let zeroOrMoreDigitList' = sepBy' digit comma
    let zeroOrMoreDigitList = sepBy' digit comma
    let oneOrMoreDigitList = sepBy1 digit comma

    let testCases  = 
        [
            ("oneOrMoreDigitList 1",     oneOrMoreDigitList,   "1",    Success (['1'], "") )
            // Testing my implementation
            ("zeroOrMoreDigitList' 1,2", zeroOrMoreDigitList', "1,2",  Success (['1'; '2'], "") )
            ("zeroOrMoreDigitList' 1",   zeroOrMoreDigitList', "1",    Success (['1'], "") )
            ("zeroOrMoreDigitList'",     zeroOrMoreDigitList', "",     Success ([ ], "") )
            // Testing the text book implementation
            ("zeroOrMoreDigitList 1,2",  zeroOrMoreDigitList, "1,2",   Success (['1'; '2'], "") )
            ("zeroOrMoreDigitList 1",    zeroOrMoreDigitList, "1",     Success (['1'], "") )
            ("zeroOrMoreDigitList",      zeroOrMoreDigitList, "",      Success ([ ], "") )
         ]

 module Section_8 =
    // =============================================
    // Section 8 - Bind
    // =============================================
    open StartingPoint
    open Section_4
    // va bindP: ('a -> Parser<'b>) -> Parser<'a> -> Parser<'b>
    let bindP f p =
        let innerFn input = 
            let result1 = run p input
            match result1 with 
            | Failure err -> 
                Failure err
            | Success (value1, remainignInput) -> 
                let p2 = f value1
                run p2 remainignInput

        Parser innerFn
    
    // va >>=: Parser<'a> -> ('a -> Parser<'b>) -> Parser<'b>
    let (>>=) p f = bindP f p

    // ----------------------------------------------------------------
    // Reimplement other functions with bindP
    // ----------------------------------------------------------------
    // val returnP: 'a -> Parser<'a>
    let returnP x =
        let innerFn input =
            Success (x, input)
        Parser innerFn

    let mapP f = bindP (f >> returnP)

    let andThen p1 p2 =         
        p1 >>= (fun p1Result -> 
        p2 >>= (fun p2Result -> 
            returnP (p1Result,p2Result) ))

    let applyP fP xP =         
        fP >>= (fun f -> 
        xP >>= (fun x -> 
            returnP (f x) ))

    let many1 p =         
        p      >>= (fun head -> 
        many p >>= (fun tail -> 
            returnP (head::tail) ))

module Test =
    // -----------------------------------------------------------------------
    // Run The Tests
    // -----------------------------------------------------------------------
    let run = StartingPoint.run
    let tests = Seq.concat [
                 Seq.ofList Section_1.testCases
                 Seq.ofList Section_2.testCases
                 Seq.ofList Section_3.testCases
                 (mapTests2 run id Section_4.testCases)
                 (mapTests2 run id Section_4.pintTestCases)
                 Seq.ofList Section_5.testCases
                 Seq.ofList Section_6.testCases
                 (mapTests2 run id Section_7.testCases)
    ]

    [<Tests>] let parserTests = testList "UsefulCombiners" tests
