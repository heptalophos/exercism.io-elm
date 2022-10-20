module CollatzConjecture exposing (collatz)

import Bitwise exposing ( shiftRightBy )


collatz : Int -> Result String Int
collatz start =
    if start < 1 then
        Err "Only positive integers are allowed"
    else  
        collatzRec start        


collatzRec : Int -> Result String Int
collatzRec start =
    if start == 1 then 
        Ok 0
    else if (start |> modBy 2) == 0 then
        Result.map ((+) 1) (collatzRec (shiftRightBy 1 start))
    else 
        Result.map ((+) 1) (collatzRec ((3 * start) + 1))
