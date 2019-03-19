module Sublist exposing (ListComparison(..), sublist)

import List exposing(foldr)
import String exposing(contains, fromInt)

type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List Int -> List Int -> ListComparison
sublist alist blist = 
    let 
        a = alist |> foldr (fromInt >> (++)) ""
        b = blist |> foldr (fromInt >> (++)) ""
    in 
        if a == b then Equal
        else if contains a b then Sublist
        else if contains b a then Superlist
        else Unequal