module Model.Types exposing (AgeCategory(..), BirthDate, YearInt(..), defaultBirthdate)

import Time


{-| A date of birth
-}
type alias BirthDate =
    Time.Posix


{-| The set of age categories this society uses
-}
type AgeCategory
    = NurseryAge
    | SchoolAge
    | WorkingAge
    | Retired


{-| An age Integer to stop the various parts of a date getting mixed up
-}
type YearInt
    = YearInt Int


defaultBirthdate : BirthDate
defaultBirthdate =
    Time.millisToPosix -2208988800000
