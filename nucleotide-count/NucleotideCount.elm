module NucleotideCount exposing (nucleotideCounts, version)

version : Int
version =
    2

type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }

nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    let 
        inc n acc =
            case n of 
                'A' -> {acc | a = acc.a + 1}
                'T' -> {acc | t = acc.t + 1}
                'C' -> {acc | c = acc.c + 1}
                'G' -> {acc | g = acc.g + 1}
                _ -> acc
    in
        sequence
        |> String.toList
        |> List.foldl inc (NucleotideCounts 0 0 0 0)