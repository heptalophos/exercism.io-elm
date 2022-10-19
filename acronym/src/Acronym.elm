module Acronym exposing ( abbreviate )

import List exposing ( map, foldr )
import String exposing ( replace, split, slice, toUpper )


abbreviate : String -> String
abbreviate =
    replace "-" " " >> split " " >> map( slice 0 1 >> toUpper )
                    >> foldr (++) ""
