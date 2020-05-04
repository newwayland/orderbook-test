module Model.TaskManager exposing (advanceTime)

import Model.Clock exposing (Clock, TimeOfDay(..))
import Model.Cursor
import Model.Individual exposing (Individual)
import Model.Individuals exposing (Individuals)
import Model.Markets exposing (Markets)
import Model.Polity exposing (Polity)
import Model.Types exposing (YearInt, AgeCategory(..))
import OrderBook exposing (OrderBook, OrderRequest)
import String.Conversions



{- The section of the model the task manager works with -}


type alias ModelElements a =
    { a
        | clock : Clock
        , individuals : Individuals
        , markets : Markets
        , polity : Polity
    }



{- Elements that are updated by the tasks -}


type alias Updateables =
    { individual : Individual
    , labour : OrderBook
    , products : OrderBook
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
            Model.Cursor.foldl
                (processIndividualActivity (morningActivity logIt))
                (morningModel model)
                model.individuals

        Midday ->
            Model.Cursor.foldl
                (processIndividualActivity (middayActivity logIt))
                (middayModel model)
                model.individuals

        Evening ->
            Model.Cursor.foldl
                (processIndividualActivity (eveningActivity logIt))
                (eveningModel model)
                model.individuals


morningModel : ModelElements a -> ModelElements a
morningModel model =
    { model
        | individuals = Model.Cursor.indexedEmpty model.individuals
        , markets = Model.Markets.empty |> Model.Cursor.retainIndex model.markets
    }


middayModel : ModelElements a -> ModelElements a
middayModel model =
    { model
        | individuals = Model.Cursor.indexedEmpty model.individuals
    }


eveningModel : ModelElements a -> ModelElements a
eveningModel model =
    { model
        | individuals = Model.Cursor.indexedEmpty model.individuals
    }


processIndividualActivity :
    (AgeCategory -> Int -> Updateables -> Updateables)
    -> Individual
    -> ModelElements a
    -> ModelElements a
processIndividualActivity processor individual model =
    let
        ageCategory =
            age model.clock individual |> Model.Polity.categoriseAge model.polity

        updates =
            Updateables individual (Model.Markets.labourMarket model.markets) (Model.Markets.productMarket model.markets) |> processor ageCategory (Model.Individual.id individual)

        updateMarket =
            Model.Markets.updateLabourMarket updates.labour >> Model.Markets.updateProductMarket updates.products
    in
    { model | individuals = Model.Cursor.push updates.individual model.individuals, markets = updateMarket model.markets }


morningActivity : Logger -> AgeCategory -> Int -> Updateables -> Updateables
morningActivity logIt ageCategory index =
    offerWork logIt ageCategory index >> askOutput logIt ageCategory index


middayActivity : Logger -> AgeCategory -> Int -> Updateables -> Updateables
middayActivity =
    buyWork


eveningActivity : Logger -> AgeCategory -> Int -> Updateables -> Updateables
eveningActivity logIt ageCategory index =
    settleWork logIt ageCategory index >> settleOutput logIt ageCategory index


settleWork : Logger -> AgeCategory -> Int -> Updateables -> Updateables
settleWork logIt ageCategory index updates =
    updates



--{ updates | individual = logIt "Getting paid for work" updates.individual }


settleOutput : Logger -> AgeCategory -> Int -> Updateables -> Updateables
settleOutput logIt ageCategory index updates =
    updates



--{ updates | individual = logIt "Paying for output" updates.individual }


offerWork : Logger -> AgeCategory -> Int -> Updateables -> Updateables
offerWork logIt ageCategory index updateables =
    let
        timeOffer =
            Model.Individual.offeredHours ageCategory updateables.individual
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
            updateables


askOutput : Logger -> AgeCategory -> Int -> Updateables -> Updateables
askOutput logIt _ index updateables =
    case
        Model.Individual.productAsk updateables.individual
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
                | individual = logIt "Broke! Can't buy anything" updateables.individual
            }


buyWork : Logger -> AgeCategory -> Int -> Updateables -> Updateables
buyWork logIt ageCategory index updateables =
    { updateables | individual = logIt "No work. Using hours myself." updateables.individual }



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



