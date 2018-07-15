module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict

type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict.Dict Int (List Student)


empty : School
empty = Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school = 
    Dict.insert grade (List.sort (student :: studentsInGrade grade school)) school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Dict.get grade school
    |> Maybe.withDefault []


allStudents : School -> List ( Grade, List Student )
allStudents school =
    List.map2 (,) (Dict.keys school) (Dict.values school)
