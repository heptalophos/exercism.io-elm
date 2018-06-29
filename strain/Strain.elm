module Strain exposing (discard, keep)

keep : (a -> Bool) -> List a -> List a
keep predicate list = case list of
    [] -> []
    x :: tail ->
        if (predicate x) then
            x :: (keep predicate tail)
        else
            keep predicate tail

discard : (a -> Bool) -> List a -> List a
discard predicate list = case list of
        [] -> []
        x :: tail ->
            if (predicate x) then
                discard predicate tail
            else
                x :: (discard predicate tail)
