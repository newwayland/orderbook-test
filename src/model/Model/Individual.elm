module Model.Individual exposing
    ( Individual, Sex(..)
    , newIndividual, addJournalEntry
    , transferCash, setId
    , id
    , defaultName, defaultIndividual
    , name, sex, birthDate, cash
    , journal
    , defaultWorkingHours, retiredWorkingHours
    , defaultWorkingPrice, retiredWorkingPrice
    , defaultProductAmount
    , offeredHours, productAsk
    , Logger
    , offeredHoursLog, unworkedHoursLog, requestedProductLog
    , noMoneyLog
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Sex


# Updaters

@docs newIndividual, addJournalEntry
@docs offer, transferCash, setId


# Queries

@docs id
@docs defaultName, defaultIndividual
@docs name, sex, birthDate, cash
@docs journal, workHoursOffered
@docs defaultWorkingHours, retiredWorkingHours
@docs defaultWorkingPrice, retiredWorkingPrice
@docs defaultProductAmount
@docs offeredHours, productAsk


# Logs

@docs Logger
@docs offeredHoursLog, unworkedHoursLog, requestedProductLog
@docs moMoneyLog

-}

import Array exposing (Array)
import Model.Types exposing (AgeCategory(..), BirthDate)



{- Biological Sex Marker -}


type Sex
    = Male
    | Female



{- a simplified model of an Individual going about their day -}


type Individual
    = Individual
        { id : Int
        , name : String
        , sex : Sex
        , birthdate : BirthDate
        , journal : Array String
        , cash : Int
        }



{- A new Individual from basic details -}


newIndividual : Int -> String -> Sex -> BirthDate -> Individual
newIndividual newId newName newSex newBirthdate =
    Individual
        { id = newId
        , name = newName
        , sex = newSex
        , birthdate = newBirthdate
        , journal = Array.empty
        , cash = 0
        }


setId : Int -> Individual -> Individual
setId index (Individual ind) =
    Individual { ind | id = index }


id : Individual -> Int
id (Individual ind) =
    ind.id


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
    newIndividual 0 defaultName Female Model.Types.defaultBirthdate



{- A default name string -}


defaultName : String
defaultName =
    "Ooops"



-- CASH TRANSACTION


transferCash : Logger -> Int -> Individual -> Individual -> ( Individual, Individual )
transferCash logIt amount from to =
    let
        amountStr =
            String.fromInt amount

        (Individual loggedFrom) =
            logIt ("Paid " ++ amountStr ++ " to " ++ name to ++ "(" ++ String.fromInt (id to) ++ ")") from

        (Individual loggedTo) =
            logIt ("Received " ++ amountStr ++ " from " ++ name to ++ "(" ++ String.fromInt (id to) ++ ")") to
    in
    ( Individual { loggedFrom | cash = loggedFrom.cash - amount }
    , Individual { loggedTo | cash = loggedTo.cash + amount }
    )



-- STORY


{-| A function type that logs a dated message in the individual's journal
-}
type alias Logger =
    String -> Individual -> Individual


{-| Add a journal entry to the journal message queue, constrained to the length of the journal
-}
addJournalEntry : String -> String -> Individual -> Individual
addJournalEntry dateTag str (Individual ind) =
    let
        element =
            dateTag ++ ": " ++ str

        qLength =
            Array.length ind.journal
    in
    Individual
        { ind
            | journal =
                (if qLength >= defaultJournalLength then
                    Array.slice 1 qLength ind.journal

                 else
                    ind.journal
                )
                    |> Array.push element
        }



{- Retrieve the journal for the individual -}


journal : Individual -> List String
journal (Individual ind) =
    Array.toList ind.journal



{- The default length of the individual journal -}


defaultJournalLength : Int
defaultJournalLength =
    10



-- Log Strings


unworkedHoursLog : Int -> String
unworkedHoursLog quantity =
    String.fromInt quantity ++ " hours unsold"


offeredHoursLog : Int -> String
offeredHoursLog quantity =
    "Offered " ++ String.fromInt quantity ++ " hours of work"


requestedProductLog : Int -> String
requestedProductLog quantity =
    "Requested " ++ String.fromInt quantity ++ " items"


noMoneyLog : String
noMoneyLog =
    "Broke! Can't buy anything"


unableToWorkLog =
    "Can't offer any hours of work"


retiredLog =
    "Retired"


atSchoolLog =
    "At school"


atNurseryLog =
    "At nursery"


schoolProductLog =
    "Living at Home"


nurseryProductLog =
    "Mum buys my things"



-- ACTIONS


{-| An amount of something at a desired price
-}
type alias Offer =
    { quantity : Int
    , price : Int
    }


{-| Does this individual want to work, and at what price?
-}
offeredHours : AgeCategory -> Individual -> ( Maybe Offer, String )
offeredHours ageCategory _ =
    case ageCategory of
        NurseryAge ->
            ( Nothing, atNurseryLog )

        SchoolAge ->
            ( Nothing, atSchoolLog )

        WorkingAge ->
            if defaultWorkingHours > 0 then
                ( Just (Offer defaultWorkingHours defaultWorkingPrice)
                , offeredHoursLog defaultWorkingHours
                )

            else
                ( Nothing, unableToWorkLog )

        Retired ->
            if retiredWorkingHours > 0 then
                ( Just (Offer retiredWorkingHours retiredWorkingPrice)
                , offeredHoursLog retiredWorkingHours
                )

            else
                ( Nothing, retiredLog )


{-| Does this individual want to buy anything, and at what price?
-}
productAsk : AgeCategory -> Individual -> ( Maybe Offer, String )
productAsk ageCategory ind =
    let
        price =
            cash ind // defaultProductAmount
    in
    case ageCategory of
        NurseryAge ->
            ( Nothing, nurseryProductLog )

        SchoolAge ->
            ( Nothing, schoolProductLog )

        WorkingAge ->
            if price <= 0 then
                ( Nothing, noMoneyLog )

            else
                ( Just (Offer defaultProductAmount price)
                , requestedProductLog defaultProductAmount
                )

        Retired ->
            if price <= 0 then
                ( Nothing, noMoneyLog )

            else
                ( Just (Offer defaultProductAmount price)
                , requestedProductLog defaultProductAmount
                )


{-| Default number of hours offered into the job pool if of working age
-}
defaultWorkingHours : Int
defaultWorkingHours =
    8


defaultWorkingPrice : Int
defaultWorkingPrice =
    1


{-| Default number of hours offered into the job pool if retired
-}
retiredWorkingHours : Int
retiredWorkingHours =
    0


retiredWorkingPrice : Int
retiredWorkingPrice =
    0


{-| Default amount of product required daily to stay alive
-}
defaultProductAmount : Int
defaultProductAmount =
    8
