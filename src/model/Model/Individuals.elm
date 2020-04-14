module Model.Individuals exposing
    ( Individuals, IndividualsArray
    , init, initFromArray, moveCursor, incrementCursor, decrementCursor
    , advanceTime
    , current, defaultLength, length
    , atMin, atMax
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individuals, IndividualsArray, Sex


# Updaters

@docs init, initFromArray, moveCursor, incrementCursor, decrementCursor
@docs advanceTime


# Queries

@docs current, defaultLength, length
@docs atMin, atMax

-}

import Array exposing (Array)
import Model.Individual exposing (Individual)
import Model.Types exposing (BirthDate)
import Queue exposing (Queue)



{- Indexed list of individuals -}


type alias IndividualsArray =
    Array Individual



{- Indexed list of individuals with a current individual cursor -}


type alias Individuals =
    { current : Int
    , individuals : IndividualsArray
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
            Model.Individual.defaultIndividual


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


{-| The length of the list of individuals
-}
length : Individuals -> Int
length inds =
    Array.length inds.individuals


{-| The index of the individual at the end of the list of individuals
-}
maxIndex : Individuals -> Int
maxIndex inds =
    length inds - 1


{-| The index of the individual at the beginning of the list of individuals
-}
minIndex : Individuals -> Int
minIndex _ =
    0


{-| The default length of the individuals list
-}
defaultLength : Int
defaultLength =
    1000


{-| Send the time tick to each individual in the list
-}
advanceTime : String -> Individuals -> Individuals
advanceTime dateTag inds =
    { inds | individuals = Array.map (Model.Individual.advanceTime dateTag) inds.individuals }
