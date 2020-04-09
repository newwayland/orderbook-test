module Model.Individual exposing
    ( Individual, Individuals, IndividualsArray, Sex(..)
    , initFromArray
    , current, defaultLength, defaultName
    , init
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Individuals, IndividualsArray, Sex


# Updaters

@docs initFromArray


# Queries

@docs current, defaultLength, defaultName

-}

import Array exposing (Array)
import Model.Types exposing (BirthDate)
import Time



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
initFromArray arr =
    Individuals 0 arr


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
