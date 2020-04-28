module Model.Markets exposing
    ( Markets
    , empty
    , labourMarket, productMarket, updateLabourMarket, updateProductMarket
    )

{-| A representation of a set of market orderbooks


# Definition

@docs Markets


# Updaters

@docs empty

-}

import Dict exposing (Dict)
import OrderBook exposing (OrderBook)


type alias Markets =
    Dict String OrderBook


marketList : List String
marketList =
    [ "labour", "products" ]


{-| A set of markets with empty OrderBooks
-}
empty : Markets
empty =
    Dict.empty


insert : String -> OrderBook -> Markets -> Markets
insert =
    Dict.insert


get : String -> Markets -> OrderBook
get key =
    Dict.get key >> Maybe.withDefault OrderBook.empty


labourMarket : Markets -> OrderBook
labourMarket =
    get "labour"


productMarket : Markets -> OrderBook
productMarket =
    get "products"


updateLabourMarket : OrderBook -> Markets -> Markets
updateLabourMarket =
    Dict.insert "labour"


updateProductMarket : OrderBook -> Markets -> Markets
updateProductMarket =
    Dict.insert "products"
