module Gigasecond exposing (add)


import Time 

gigasec : Int
gigasec = 
    10 ^ 9

add : Time.Posix -> Time.Posix
add =
    Time.posixToMillis 
    >> (+) (gigasec * 10 ^ 3)
    >> Time.millisToPosix 