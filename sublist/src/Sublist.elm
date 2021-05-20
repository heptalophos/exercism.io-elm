module Sublist exposing (ListComparison(..), sublist)

import List exposing(foldr)
import String exposing(contains)

type ListComparison = Equal
                      | Superlist
                      | Sublist
                      | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist = 
    let 
        stringify = foldr (Debug.toString >> (++)) ""
        xs = alist |> stringify
        ys = blist |> stringify
    in 
        if xs == ys then Equal
        else if contains xs ys then Sublist
        else if contains ys xs then Superlist
        else Unequal
