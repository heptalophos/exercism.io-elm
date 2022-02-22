module Sublist exposing (ListComparison(..), sublist)


import List exposing(foldr)
import String exposing(contains)


type ListComparison 
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist = 
    let 
        partOf xs ys = 
            List.range 0 (List.length ys - List.length xs)
            |> List.any (\x -> 
                         List.map2 (==) (List.drop x ys) xs
                         |> List.all (\y -> y))
    in 
        case ((partOf alist blist), (partOf blist alist)) of
            (True, True)   -> Equal
            (True, False)  -> Sublist
            (False, True)  -> Superlist
            (False, False) -> Unequal
