module Bob exposing (hey)

import Char exposing (isLower, isUpper)
import String exposing (all, endsWith, filter, isEmpty, trim)

hey : String -> String
hey remark = let 
                trimmed =
                    String.trim remark 
             in
                if (shouting trimmed && question trimmed) then
                    "Calm down, I know what I'm doing!"
                else if shouting trimmed then
                    "Whoa, chill out!"
                else if question trimmed then 
                    "Sure."
                else if silence trimmed then
                    "Fine. Be that way!"
                else
                    "Whatever."
    

silence : String -> Bool
silence = isEmpty << trim

shouting : String -> Bool
shouting msg = all isUpper (filter isAlpha msg) && not (isEmpty (filter isAlpha msg)) 

question : String -> Bool
question = endsWith "?"

isAlpha : Char -> Bool
isAlpha c = isLower (c) || isUpper (c)