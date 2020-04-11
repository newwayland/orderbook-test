module Model.Individual exposing
    ( Individual, Individuals, IndividualsArray, Sex(..)
    , initFromArray, moveCursor
    , current, defaultLength, defaultName, length
    , init
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Individuals, IndividualsArray, Sex


# Updaters

@docs initFromArray, moveCursor


# Queries

@docs current, defaultLength, defaultName, length

-}

import Array exposing (Array)
import Model.Types exposing (BirthDate)



{- Indexed list of individuals -}


type alias IndividualsArray =
    Array Individual



{- Indexed list of individuals with a current individual cursor -}


type alias Individuals =
    { current : Int
    , individuals : IndividualsArray
    }



{- Biological Sex Marker -}


type Sex
    = Male
    | Female



{- a simplified model of an Individual going about their day -}


type alias Individual =
    { name : String
    , sex : Sex
    , birthdate : BirthDate
    }


init : Individuals
init =
    initFromArray Array.empty


{-| Build a cursored individal from an indexed list of individual
-}
initFromArray : IndividualsArray -> Individuals
initFromArray =
    Individuals 0


{-| Change the current individual
-}
moveCursor : Individuals -> Int -> Individuals
moveCursor inds value =
    { inds | current = clamp 0 (Array.length inds.individuals - 1) value }


{-| Use the cursor value to extract the current individual from the set
of individuals supplied
-}
current : Individuals -> Individual
current individuals =
    case Array.get individuals.current individuals.individuals of
        Just value ->
            value

        Nothing ->
            defaultIndividual


{-| The length of the list of individuals
-}
length : Individuals -> Int
length inds =
    Array.length inds.individuals



{- The default individual - returned when the array is empty -}


defaultIndividual : Individual
defaultIndividual =
    Individual defaultName Female Model.Types.defaultBirthdate



{- The default length of the individuals list -}


defaultLength : Int
defaultLength =
    1000



{- A default name string -}


defaultName : String
defaultName =
    "Ooops"
