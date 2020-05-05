module Model.Polity exposing (Polity, categoriseAge, default, schoolAge, majority, retirementAge)

import Model.Types exposing (AgeCategory(..), YearInt(..))


type alias Polity =
    { retirementAge : YearInt
    , majority : YearInt
    , schoolAge : YearInt
    }



default : Polity
default =
    { retirementAge = YearInt 65
    , majority = YearInt 18
    , schoolAge = YearInt 5
    }



-- Age Checks


categoriseAge : Polity -> YearInt -> AgeCategory
categoriseAge pol (YearInt age) =
    if age < schoolAge pol then
        NurseryAge

    else if age < majority pol then
        SchoolAge

    else if age < retirementAge pol then
        WorkingAge

    else
        Retired


fromInt : Int -> YearInt
fromInt =
    YearInt

schoolAge : Polity -> Int
schoolAge  pol =
    let (YearInt age) = pol.schoolAge
    in
        age

majority : Polity -> Int
majority pol =
    let (YearInt age) = pol.majority
    in
        age

retirementAge : Polity -> Int
retirementAge pol =
    let (YearInt age) = pol.retirementAge
    in
        age
