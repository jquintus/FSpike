open System

let A_Parser str = 
    if String.IsNullOrEmpty(str) then
        false, ""
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        true, remaining
    else
        false, str