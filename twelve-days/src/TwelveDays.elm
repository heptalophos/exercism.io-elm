module TwelveDays exposing (recite)

import List   exposing (range, reverse, map)
import String exposing (join)


recite : Int -> Int -> List String
recite start stop =
    range start stop |> map verse


-- Auxiliary

verse : Int -> String
verse day =
    let
        premise = 
            "On the " ++ nth day ++ " day of Christmas " ++ 
            "my true love gave to me: "
        conclusion = 
            range 1 day |> reverse |> map (gifts day) |> join ", "
    in
        premise ++ conclusion ++ "."


gifts : Int -> Int -> String
gifts thisManyOn day =
    let
        prefix =
            if thisManyOn /= 1 && day == 1 then "and "
            else ""
    in
        prefix ++ howManyOn day ++ " " ++ giftsOn day


giftsOn : Int -> String
giftsOn day =
    case day of
        0  -> "Partridge in a Pear Tree"
        1  -> "Partridge in a Pear Tree"
        2  -> "Turtle Doves"
        3  -> "French Hens"
        4  -> "Calling Birds"
        5  -> "Gold Rings"
        6  -> "Geese-a-Laying"
        7  -> "Swans-a-Swimming"
        8  -> "Maids-a-Milking"
        9  -> "Ladies Dancing"
        10 -> "Lords-a-Leaping"
        11 -> "Pipers Piping"
        12 -> "Drummers Drumming"
        _  -> "unknown gift"


howManyOn : Int -> String
howManyOn day =
    case day of
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


nth : Int -> String
nth day =
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

