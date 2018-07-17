module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive numbers are allowed"
    else 
        Ok (collatzExec 0 start)


collatzExec : Int -> Int -> Int
collatzExec steps start =
    if start == 1 then 
        steps
    else if start % 2 == 0 then
        collatzExec (steps + 1) (start // 2)
    else 
        collatzExec (steps + 1) (3 * start + 1)
