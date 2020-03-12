module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive numbers are allowed"
    else 
        Ok (collatzRec 0 start)


collatzRec : Int -> Int -> Int
collatzRec steps start =
    if start == 1 then 
        steps
    else if start % 2 == 0 then
        collatzRec (steps + 1) (start // 2)
    else 
        collatzRec (steps + 1) (3 * start + 1)
