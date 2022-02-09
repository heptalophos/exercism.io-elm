module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start < 1 then
        Err "Only positive integers are allowed"
    else  
        collatzSteps start        


collatzSteps : Int -> Result String Int
collatzSteps start =
    if start == 1 then 
        Ok 0
    else if (start |> modBy 2) == 0 then
        Result.map ((+) 1) (collatzSteps (start // 2))
    else 
        Result.map ((+) 1) (collatzSteps ((3 * start) + 1))
