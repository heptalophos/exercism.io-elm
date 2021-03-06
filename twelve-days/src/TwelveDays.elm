module TwelveDays exposing (recite)

cardinal : Int -> String
cardinal n =
    case n of
        1  -> "a"
        2  -> "two"
        3  -> "three"
        4  -> "four"
        5  -> "five"
        6  -> "six"
        7  -> "seven"
        8  -> "eight"
        9  -> "nine"
        10 -> "ten"
        11 -> "eleven"
        12 -> "twelve"
        _  -> "unknown number"


ordinal : Int -> String
ordinal day =
    case day of
        1  -> "first"
        2  -> "second"
        3  -> "third"
        4  -> "fourth"
        5  -> "fifth"
        6  -> "sixth"
        7  -> "seventh"
        8  -> "eighth"
        9  -> "ninth"
        10 -> "tenth"
        11 -> "eleventh"
        12 -> "twelfth"
        _  -> "unknown count"


giftOn : Int -> String
giftOn n =
    case n of
        0 -> "Partridge in a Pear Tree"
        1 -> "Partridge in a Pear Tree"
        2 -> "Turtle Doves"
        3 -> "French Hens"
        4 -> "Calling Birds"
        5 -> "Gold Rings"
        6 -> "Geese-a-Laying"
        7 -> "Swans-a-Swimming"
        8 -> "Maids-a-Milking"
        9 -> "Ladies Dancing"
        10 -> "Lords-a-Leaping"
        11 -> "Pipers Piping"
        12 -> "Drummers Drumming"
        _ -> "unknown subject"

gifts : Int -> Int -> String
gifts howMany day =
    let
        prefix =
            if howMany /= 1 && day == 1 then
                "and "
            else
                ""
    in
        prefix ++ cardinal day ++ " " ++ giftOn day

premise : Int -> String
premise n =
    "On the " ++ 
    ordinal n ++ 
    " day of Christmas " ++ 
    "my true love gave to me: "

conclusion : Int -> String
conclusion n =
    List.range 1 n
    |> List.reverse
    |> List.map (gifts n)
    |> String.join ", "

verse : Int -> String
verse n =
    premise n ++ conclusion n ++ "."

recite : Int -> Int -> List String
recite start stop =
    List.range start stop
    |> List.map verse
