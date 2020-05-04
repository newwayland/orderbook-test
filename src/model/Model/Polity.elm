module Model.Polity exposing (Polity, categoriseAge, default)

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
    let
        (YearInt retirementAge) =
            pol.retirementAge

        (YearInt majority) =
            pol.majority

        (YearInt schoolAge) =
            pol.schoolAge
    in
    if age < schoolAge then
        NurseryAge

    else if age < majority then
        SchoolAge

    else if age < retirementAge then
        WorkingAge

    else
        Retired
