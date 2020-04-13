module Model.Individual exposing
    ( Individual, Individuals, IndividualsArray, Sex(..)
    , newIndividual
    , initFromArray, moveCursor, incrementCursor, decrementCursor
    , current, defaultLength, defaultName, length
    , atMin, atMax
    , name, sex, birthDate
    , init
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Individuals, IndividualsArray, Sex


# Updaters

@docs newIndividual
@docs initFromArray, moveCursor, incrementCursor, decrementCursor


# Queries

@docs current, defaultLength, defaultName, length
@docs atMin, atMax
@docs name, sex, birthDate

-}

import Array exposing (Array)
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



{- Biological Sex Marker -}


type Sex
    = Male
    | Female



{- a simplified model of an Individual going about their day -}


type Individual
    = Individual
        { name : String
        , sex : Sex
        , birthdate : BirthDate
        , journal : Queue String
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


name : Individual -> String
name (Individual ind) =
    ind.name


sex : Individual -> Sex
sex (Individual ind) =
    ind.sex


birthDate : Individual -> BirthDate
birthDate (Individual ind) =
    ind.birthdate



{- A new Individual from basic details -}


newIndividual : String -> Sex -> BirthDate -> Individual
newIndividual newName newSex newBirthdate =
    Individual { name = newName, sex = newSex, birthdate = newBirthdate, journal = Queue.empty }



{- The default individual - returned when the array is empty -}


defaultIndividual : Individual
defaultIndividual =
    newIndividual defaultName Female Model.Types.defaultBirthdate



{- The default length of the individuals list -}


defaultLength : Int
defaultLength =
    1000



{- A default name string -}


defaultName : String
defaultName =
    "Ooops"



{- The default length of the individual journal -}


defaultJournalLength : Int
defaultJournalLength =
    10



{- Add a journal entry to the journal message queue, constrained to the length of the journal -}


addJournalEntry : String -> Individual -> Individual
addJournalEntry element (Individual ind) =
    let
        newQ =
            Queue.enqueue element ind.journal
    in
    Individual
        { ind
            | journal =
                Queue.drop (Queue.length newQ - defaultJournalLength) newQ
        }



{- Retrieve the journal for the individual -}


journal : Individual -> List String
journal (Individual ind) =
    Queue.toList ind.journal
