module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )


length : List a -> Int
length list =
    list |> foldl (\_ -> (+) 1) 0

reverse : List a -> List a
reverse list =
    list |> foldl (::) []


foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    case list of
        x :: xs -> foldl f (f x acc) xs 
        []      -> acc


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    list |> reverse |> foldl f acc 


map : (a -> b) -> List a -> List b
map f list =
    list |> foldr (\x acc -> f x :: acc) [] 


filter : (a -> Bool) -> List a -> List a
filter f list =
    case list of 
        x :: xs -> 
            case f x of 
                True -> x :: filter f xs
                _    -> filter f xs
        []      -> list 


append : List a -> List a -> List a
append xs ys =
    xs |> (foldr (::) <| ys)


concat : List (List a) -> List a
concat list =
    list |> foldr append [] 
