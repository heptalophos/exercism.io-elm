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
        validStrand = sequence 
                      |> String.toList
                      |> List.all (
                            \n -> List.member n ("ATCG" |> String.toList)
                         ) 
    in 
        case validStrand of 
            True -> Ok  { a = String.indices "A" sequence |> List.length
                        , t = String.indices "T" sequence |> List.length
                        , c = String.indices "C" sequence |> List.length
                        , g = String.indices "G" sequence |> List.length
                        }
            _    -> Err "Invalid nucleotide in strand" 
