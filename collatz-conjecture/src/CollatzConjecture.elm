module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start < 1 then
        Err "Only positive numbers are allowed"
    else  
        collatzRec start        


collatzRec : Int -> Result String Int
collatzRec start =
    if start == 1 then 
        Ok 0
    else if (start |> remainderBy 2) == 0 then
        Result.map ((+) 1) 
                   (collatzRec (start // 2))
    else 
        Result.map ((+) 1) 
                   (collatzRec ((3 * start) + 1))
