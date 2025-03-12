module GradeSchool exposing
            ( Grade
            , Result(..)
            , School
            , Student
            , addStudent
            , allStudents
            , emptySchool
            , studentsInGrade
            )

import Dict exposing ( Dict, empty, get, insert, values)
import List exposing (sort, concat, member)
import Maybe exposing (withDefault)

type alias Grade = Int

type alias Student = String

type alias School = Dict Grade (List Student)

type Result
    = Added
    | Duplicate

emptySchool : School
emptySchool =
    empty

addStudent : Grade -> Student -> School -> (Result, School)
addStudent grade student school =
    let 
        inSchool =  
            insert grade 
                (sort (student :: studentsInGrade grade school))
                school
        duplicateStudent = 
            member student (allStudents school)
    in
        if duplicateStudent 
        then (Duplicate, school) 
        else (Added, inSchool)

studentsInGrade : Grade -> School -> List Student
studentsInGrade grade =
    get grade >> withDefault []

allStudents : School -> List Student
allStudents school =
    school |> concat << values
