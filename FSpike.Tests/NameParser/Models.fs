module NameParserModels

type FullName = 
    {
        FirstName : string
        MiddleName : string option
        LastName  :string
    }

let createName2 first last        = {FirstName = first; MiddleName = None;        LastName = last }
let createName3 first middle last = {FirstName = first; MiddleName = Some middle; LastName = last }

let format name = 
    let middleName = match name.MiddleName with
                     | Some n -> n
                     | None -> System.String.Empty

    [ name.FirstName; middleName; name.LastName ] 
    |> Seq.filter (fun n -> n.Length > 0)
    |>String.concat " "


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