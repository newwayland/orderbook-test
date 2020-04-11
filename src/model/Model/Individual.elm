module Model.Individual exposing
    ( Individual, Individuals, IndividualsArray, Sex(..)
    , initFromArray, moveCursor, incrementCursor, decrementCursor
    , current, defaultLength, defaultName, length
    , atMin, atMax
    , init
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Individuals, IndividualsArray, Sex


# Updaters

@docs initFromArray, moveCursor, incrementCursor, decrementCursor


# Queries

@docs current, defaultLength, defaultName, length
@docs atMin, atMax

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
    { inds | current = clamp (minIndex inds) (maxIndex inds) value }


{-| Decrease the cursor
-}
decrementCursor : Individuals -> Individuals
decrementCursor inds =
    moveCursor inds (inds.current - 1)


{-| Increase the cursor
-}
incrementCursor : Individuals -> Individuals
incrementCursor inds =
    moveCursor inds (inds.current + 1)


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


{-| Is the cursor at the minimum index?
-}
atMin : Individuals -> Bool
atMin inds =
    inds.current == minIndex inds


{-| Is the cursor at the maximum index?
-}
atMax : Individuals -> Bool
atMax inds =
    inds.current == maxIndex inds


maxIndex : Individuals -> Int
maxIndex inds =
    length inds - 1


minIndex : Individuals -> Int
minIndex _ =
    0



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
