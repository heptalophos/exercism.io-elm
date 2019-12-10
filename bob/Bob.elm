module Bob exposing (hey)

import Char exposing (isLower, isUpper)
import String exposing (all, endsWith, filter, isEmpty, trim)

hey : String -> String
hey remark = if (shouting remark && question remark) then
                "Calm down, I know what I'm doing!"
             else if shouting remark then
                "Whoa, chill out!"
             else if question remark then 
                "Sure."
             else if silence remark then
                "Fine. Be that way!"
             else
                "Whatever."

silence : String -> Bool
silence = isEmpty << trim

shouting : String -> Bool
shouting msg = all isUpper (filter isAlpha msg) 
               && not (isEmpty (filter isAlpha msg)) 

question : String -> Bool
question = trim >> endsWith "?"

isAlpha : Char -> Bool
isAlpha c = isLower (c) || isUpper (c)