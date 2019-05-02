module NucleotideCount2 exposing (nucleotideCounts, version)

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
    { a = String.indices "A" sequence |> List.length
    , t = String.indices "T" sequence |> List.length
    , c = String.indices "C" sequence |> List.length
    , g = String.indices "G" sequence |> List.length
    }