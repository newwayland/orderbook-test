module Model.Types exposing (BirthDate, defaultBirthdate)

import Time



{- A date of birth -}


type alias BirthDate =
    Time.Posix


defaultBirthdate : BirthDate
defaultBirthdate =
    Time.millisToPosix -2208988800000
