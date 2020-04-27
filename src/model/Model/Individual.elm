module Model.Individual exposing
    ( Individual, Sex(..)
    , newIndividual, addJournalEntry
    , defaultName, defaultIndividual
    , name, sex, birthDate, cash
    , journal
    , defaultWorkingHours, retiredWorkingHours
    , defaultWorkingPrice, retiredWorkingPrice
    , defaultProductAmount
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Sex


# Updaters

@docs newIndividual, addJournalEntry
@docs offer


# Queries

@docs defaultName, defaultIndividual
@docs name, sex, birthDate, cash
@docs journal, workHoursOffered
@docs defaultWorkingHours, retiredWorkingHours
@docs defaultWorkingPrice, retiredWorkingPrice
@docs defaultProductAmount

-}

import Model.Types exposing (BirthDate)
import Queue exposing (Queue)



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
        , cash : Int
        }



{- A new Individual from basic details -}


newIndividual : String -> Sex -> BirthDate -> Individual
newIndividual newName newSex newBirthdate =
    Individual
        { name = newName
        , sex = newSex
        , birthdate = newBirthdate
        , journal = Queue.empty
        , cash = 0
        }


name : Individual -> String
name (Individual ind) =
    ind.name


sex : Individual -> Sex
sex (Individual ind) =
    ind.sex


birthDate : Individual -> BirthDate
birthDate (Individual ind) =
    ind.birthdate


cash : Individual -> Int
cash (Individual ind) =
    ind.cash



{- The default individual - returned when the array is empty -}


defaultIndividual : Individual
defaultIndividual =
    newIndividual defaultName Female Model.Types.defaultBirthdate



{- A default name string -}


defaultName : String
defaultName =
    "Ooops"



{- The default length of the individual journal -}


defaultJournalLength : Int
defaultJournalLength =
    10



{- Default number of hours offered into the job pool if of working age -}


defaultWorkingHours : Int
defaultWorkingHours =
    8


defaultWorkingPrice : Int
defaultWorkingPrice =
    10



{- Default number of hours offered into the job pool if retired -}


retiredWorkingHours : Int
retiredWorkingHours =
    0


retiredWorkingPrice : Int
retiredWorkingPrice =
    0



{- Default amount of product required daily to stay alive -}


defaultProductAmount =
    8



{- Add a journal entry to the journal message queue, constrained to the length of the journal -}


addJournalEntry : String -> String -> Individual -> Individual
addJournalEntry dateTag str (Individual ind) =
    let
        element =
            dateTag ++ ": " ++ str

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
