module ScrabbleScore exposing (scoreWord)

import Dict exposing(fromList, get) 

letterScores : Dict.Dict String Int
letterScores = Dict.fromList [("A",1),("B",3),("C",3),("D",2),("E",1),("F",4),("G",2),("H",4),("I",1),("J",8),("K",5),("L",1),
            ("M",3),("N",1),("O",1),("P",3),("Q",10),("R",1),("S",1),("T",1),("U",1),("V",4),("W",4),("X",8),("Y",4),("Z",10)]

value : String -> Int
value ch = Maybe.withDefault 0 (Dict.get ch letterScores)

scoreWord : String -> Int
scoreWord x = String.toUpper x
                |> String.split ""
                |> List.foldl (\n acc -> acc + (value n)) 0
                -- or alternatively...
                -- |> List.map value
                -- |> List.sum
   
