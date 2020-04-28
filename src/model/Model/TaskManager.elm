module Model.TaskManager exposing (advanceTime)

import Model.Clock exposing (Clock, TimeOfDay(..), YearInt, isNurseryAge, isRetired, isSchoolAge, isWorkingAge)
import Model.Individual exposing (Individual)
import Model.Individuals exposing (Individuals)
import Model.Markets exposing (Markets)
import OrderBook exposing (OrderBook, OrderRequest)
import String.Conversions



{- The section of the model the task manager works with -}


type alias ModelElements a =
    { a
        | clock : Clock
        , individuals : Individuals
        , markets : Markets
    }



{- Elements that are updated by the tasks -}


type alias Updateables =
    { individual : Individual
    , labour : OrderBook
    , products : OrderBook
    }


type alias Desire =
    { quantity : Int
    , price : Int
    }



{- A function that logs a message in the individual's journal -}


type alias Logger =
    String -> Individual -> Individual



{- Move the model on for the current time of day -}


advanceTime : ModelElements a -> ModelElements a
advanceTime model =
    let
        timeOfDay =
            Model.Clock.toTimeOfDay model.clock

        logIt =
            Model.Clock.toDateTime model.clock |> dateTagView >> Model.Individual.addJournalEntry
    in
    case timeOfDay of
        Morning ->
            Model.Individuals.foldlIndexed
                (processIndividualActivity (morningActivity logIt))
                (morningModel model)
                model.individuals

        Midday ->
            Model.Individuals.foldlIndexed
                (processIndividualActivity (middayActivity logIt))
                (middayModel model)
                model.individuals

        Evening ->
            Model.Individuals.foldlIndexed
                (processIndividualActivity (eveningActivity logIt))
                (eveningModel model)
                model.individuals


morningModel : ModelElements a -> ModelElements a
morningModel model =
    { model
        | individuals = Model.Individuals.indexedEmpty model.individuals
        , markets = Model.Markets.empty
    }


middayModel : ModelElements a -> ModelElements a
middayModel model =
    { model
        | individuals = Model.Individuals.indexedEmpty model.individuals
    }


eveningModel : ModelElements a -> ModelElements a
eveningModel model =
    { model
        | individuals = Model.Individuals.indexedEmpty model.individuals
    }


processIndividualActivity :
    (YearInt -> Int -> Updateables -> Updateables)
    -> ( Int, Individual )
    -> ModelElements a
    -> ModelElements a
processIndividualActivity processor ( index, individual ) model =
    let
        currentAge =
            age model.clock individual

        updates =
            Updateables individual (Model.Markets.labourMarket model.markets) (Model.Markets.productMarket model.markets) |> processor currentAge index

        updateMarket =
            Model.Markets.updateLabourMarket updates.labour >> Model.Markets.updateProductMarket updates.products
    in
    { model | individuals = Model.Individuals.push updates.individual model.individuals, markets = updateMarket model.markets }


morningActivity : Logger -> YearInt -> Int -> Updateables -> Updateables
morningActivity logIt currentAge index =
    offerWork logIt currentAge index >> askOutput logIt currentAge index


middayActivity : Logger -> YearInt -> Int -> Updateables -> Updateables
middayActivity =
    buyWork


eveningActivity : Logger -> YearInt -> Int -> Updateables -> Updateables
eveningActivity logIt currentAge index =
    settleWork logIt currentAge index >> settleOutput logIt currentAge index


settleWork : Logger -> YearInt -> Int -> Updateables -> Updateables
settleWork logIt currentAge index updates =
    { updates | individual = logIt "Getting paid for work" updates.individual }


settleOutput : Logger -> YearInt -> Int -> Updateables -> Updateables
settleOutput logIt currentAge index updates =
    { updates | individual = logIt "Paying for output" updates.individual }


offerWork : Logger -> YearInt -> Int -> Updateables -> Updateables
offerWork logIt currentAge index updateables =
    let
        timeOffer =
            offeredHours currentAge updateables.individual
    in
    case timeOffer of
        Just desiredWork ->
            { updateables
                | individual = logIt ("Offered " ++ String.fromInt desiredWork.quantity ++ " Hours of Work") updateables.individual
                , labour =
                    OrderBook.sell
                        (OrderRequest index desiredWork.quantity (Just desiredWork.price))
                        updateables.labour
            }

        Nothing ->
            { updateables
                | individual = logIt "Not Working" updateables.individual
            }


askOutput : Logger -> YearInt -> Int -> Updateables -> Updateables
askOutput logIt _ index updateables =
    case
        productAsk updateables.individual
    of
        Just desiredProduct ->
            { updateables
                | individual = logIt ("Requested " ++ String.fromInt desiredProduct.quantity ++ " Items") updateables.individual
                , products =
                    OrderBook.buy
                        (OrderRequest index desiredProduct.quantity (Just desiredProduct.price))
                        updateables.products
            }

        Nothing ->
            { updateables
                | individual = logIt "Unable to ask for any Items" updateables.individual
            }


buyWork : Logger -> YearInt -> Int -> Updateables -> Updateables
buyWork logIt currentAge index updateables =
    { updateables | individual = logIt "Working" updateables.individual }



-- HELPERS


{-| Calculate the age of an individual in years
-}
age : Clock -> Individual -> YearInt
age clock =
    Model.Individual.birthDate >> Model.Clock.age clock


{-| Format the current clock as a string for use in journal entries
-}
dateTagView : Model.Clock.DateTime -> String
dateTagView dateTime =
    let
        year =
            String.fromInt dateTime.year |> String.padLeft 4 '0'

        month =
            String.Conversions.fromMonth dateTime.month

        day =
            String.fromInt dateTime.day |> String.padLeft 2 ' '
    in
    [ day, month, year ] |> String.join " "


{-| Does this individual want to work, and at what price?
-}
offeredHours : YearInt -> Individual -> Maybe Desire
offeredHours currentAge ind =
    if isWorkingAge currentAge && Model.Individual.defaultWorkingHours > 0 then
        Just (Desire Model.Individual.defaultWorkingHours Model.Individual.defaultWorkingPrice)

    else if isRetired currentAge && Model.Individual.retiredWorkingHours > 0 then
        Just (Desire Model.Individual.retiredWorkingHours Model.Individual.retiredWorkingPrice)

    else
        Nothing


productAsk : Individual -> Maybe Desire
productAsk ind =
    let
        price =
            Model.Individual.cash ind // Model.Individual.defaultProductAmount
    in
    if price > 0 then
        Just (Desire Model.Individual.defaultProductAmount price)

    else
        Nothing
