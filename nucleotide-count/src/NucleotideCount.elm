module NucleotideCount exposing (nucleotideCounts)

type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }

nucleotideCounts : String -> Result String NucleotideCounts
nucleotideCounts sequence =
    let 
        validStrand = 
            sequence 
            |> String.toList
            |> List.all (\n -> "ATCG" |> String.toList 
                                      |> List.member n) 
        countOf n strand = 
            strand |> String.indices n |> List.length
    in 
        case validStrand of 
            True -> 
                Ok  { a = sequence |> countOf "A" 
                    , t = sequence |> countOf "T"
                    , c = sequence |> countOf "C"
                    , g = sequence |> countOf "G"
                    }
            _    -> 
                Err "Invalid nucleotide in strand" 
