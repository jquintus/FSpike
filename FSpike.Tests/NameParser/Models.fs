module NameParserModels

type FullName = 
    {
        FirstName : string
        MiddleName : string option
        LastName  :string
    }

let createName2 first last = {FirstName = first; LastName = last; MiddleName = None }
let createName3 first middle last = {FirstName = first; MiddleName = Some middle; LastName = last }

let format name = 
    let middleName = match name.MiddleName with
                     | Some n -> sprintf "%s " n
                     | None -> ""

    sprintf "%s %s%s" name.FirstName middleName name.LastName 


module Tests =
    open Fuchu

    let josh = createName3 "Josh" "Paul" "Quintus"
    let ksenia = createName2 "Ksenia" "Winnicki"
    
    let nameFormatingTestCases =    
        [ 
            ( "Josh", josh, "Josh Paul Quintus") 
            ("Ksenia", ksenia, "Ksenia Winnicki") 
        ]
    [<Tests>]
    let tupledTests =
        testList "NameParserModels Tests" (mapTests1 format id nameFormatingTestCases)