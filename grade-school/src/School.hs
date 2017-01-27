module School (School, add, empty, grade, sorted) where

import Data.Maybe (fromJust, isNothing)
import Data.List (sort, find, delete)

type School = [(Int, [String])]

add :: Int -> String -> School -> School
add grade name school =
    let findGrade = find (isGrade) school; isGrade g = grade == fst g
    in if isNothing findGrade
        then (grade, [name]) : school
        else
            let oldGrade = fromJust findGrade
                newGrade = (fst oldGrade, sort $ name : snd oldGrade)
            in newGrade : delete oldGrade school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade n school =
    let lookGrade = lookup n school
    in if isNothing lookGrade
        then []
        else fromJust lookGrade

sorted :: School -> School
sorted school = sort school
