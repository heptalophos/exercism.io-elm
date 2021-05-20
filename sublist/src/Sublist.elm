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
        case ((contains xs ys), (contains ys xs)) of
            (True, True)   -> Equal
            (True, False)  -> Sublist
            (False, True)  -> Superlist
            (False, False) -> Unequal
