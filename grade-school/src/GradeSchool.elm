module GradeSchool exposing (addStudent, 
                             allStudents, 
                             empty, 
                             studentsInGrade)

import List exposing (sort, map2)
import Dict exposing (Dict, get, empty, 
                      insert, keys, values)
import Tuple exposing (pair)
import Maybe exposing (withDefault)


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
    insert grade (sort 
                 (student :: studentsInGrade grade school)) 
                 school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    get grade school
    |> withDefault []

allStudents : School -> List ( Grade, List Student )
allStudents school =
    map2 pair (keys school) (values school)
