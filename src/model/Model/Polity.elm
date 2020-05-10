module Model.Polity exposing
    ( Polity
    , categoriseAge
    , changeMajority
    , changePopulation
    , changeRetirementAge
    , changeSchoolAge
    , default
    , majority
    , retirementAge
    , schoolAge
    )

import Model.Types exposing (AgeCategory(..), YearInt(..))


type alias Polity =
    { retirementAge : YearInt
    , majority : YearInt
    , schoolAge : YearInt
    , population : Int
    }


default : Polity
default =
    { retirementAge = YearInt 65
    , majority = YearInt 18
    , schoolAge = YearInt 5
    , population = 1000
    }


changePopulation : Polity -> Int -> Polity
changePopulation pol popn =
    { pol | population = popn }



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


schoolAge : Polity -> Int
schoolAge pol =
    let
        (YearInt age) =
            pol.schoolAge
    in
    age


majority : Polity -> Int
majority pol =
    let
        (YearInt age) =
            pol.majority
    in
    age


retirementAge : Polity -> Int
retirementAge pol =
    let
        (YearInt age) =
            pol.retirementAge
    in
    age


changeSchoolAge : Maybe Int -> Polity -> Polity
changeSchoolAge age pol =
    case age of
        Just newAge ->
            { pol | schoolAge = YearInt newAge }

        Nothing ->
            pol


changeMajority : Maybe Int -> Polity -> Polity
changeMajority age pol =
    case age of
        Just newAge ->
            { pol | majority = YearInt newAge }

        Nothing ->
            pol


changeRetirementAge : Maybe Int -> Polity -> Polity
changeRetirementAge age pol =
    case age of
        Just newAge ->
            { pol | retirementAge = YearInt newAge }

        Nothing ->
            pol
