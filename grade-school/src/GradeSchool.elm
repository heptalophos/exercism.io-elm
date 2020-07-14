module GradeSchool exposing
    ( addStudent
    , allStudents
    , empty
    , studentsInGrade
    )

import Dict
    exposing
        ( Dict
        , get
        , insert
        , keys
        , values
        )
import List exposing (map2, sort)
import Maybe exposing (withDefault)
import Tuple exposing (pair)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Grade (List Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    insert grade
        (sort
            (student :: studentsInGrade grade school)
        )
        school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    get grade school
        |> withDefault []


allStudents : School -> List ( Grade, List Student )
allStudents school =
    map2 pair (keys school) (values school)
