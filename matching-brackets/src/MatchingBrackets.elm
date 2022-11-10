module MatchingBrackets exposing (isPaired)

import String exposing (contains, fromChar, toList)


isPaired : String -> Bool
isPaired input =
    paired (input |> toList) []


paired : List Char -> List Char -> Bool
paired s1 s2 = 
    case (s1, s2) of
        ([], []) -> True
        ([], _)  -> False
        ((']'::xs), ('['::ys)) -> paired xs ys
        ((')'::xs), ('('::ys)) -> paired xs ys
        (('}'::xs), ('{'::ys)) -> paired xs ys
        ((x::xs), stack) -> 
            case x of
                '}' -> False
                ')' -> False
                ']' -> False
                '[' -> paired xs (x::stack)
                '{' -> paired xs (x::stack)
                '(' -> paired xs (x::stack)
                _   -> paired xs stack
