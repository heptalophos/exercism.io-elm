module BinarySearch exposing (find)

import Array exposing (Array, get, length)


find : Int -> Array Int -> Maybe Int
find target xs =
    let 
        search low high =
            let 
                mid = (low + high) // 2
            in 
                case xs |> (get <| mid) of
                    Nothing ->
                        Nothing
                    Just x ->
                        if low > high then Nothing
                        else if x < target then search (mid + 1) high     
                        else if x > target then search low (mid - 1) 
                        else Just mid
    in
        search 0 ((xs |> length) - 1)