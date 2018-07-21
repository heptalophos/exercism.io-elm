module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive numbers are allowed"
    else 
        Ok (collatzConvergence 0 start)


collatzConvergence : Int -> Int -> Int
collatzConvergence steps start =
    if start == 1 then 
        steps
    else if start % 2 == 0 then
        collatzConvergence (steps + 1) (start // 2)
    else 
        collatzConvergence (steps + 1) (3 * start + 1)
