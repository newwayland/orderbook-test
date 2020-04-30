module View.Market exposing (card)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Model.Cursor
import Model.Markets exposing (Markets)
import OrderBook exposing (Order)
import String.Extra
import Update exposing (Msg(..))


card : String -> Markets -> Accordion.Card Msg
card seq markets =
    Accordion.card
        { id = seq
        , options = [ Card.align Text.alignXsCenter ]
        , header = marketCardHeader markets
        , blocks =
            [ Accordion.block [ Block.align Text.alignXsLeft ] (viewForm markets)
            ]
        }


marketCardHeader : Markets -> Accordion.Header Msg
marketCardHeader markets =
    Accordion.toggle [ style "min-width" "75%" ]
        [ Button.button
            [ Button.outlinePrimary
            , Button.large
            , Button.block
            ]
            [ currentMarketName markets |> displayTitle |> text ]
        ]
        |> Accordion.header []
        |> Accordion.appendHeader
            [ Button.button
                [ Button.primary
                , Button.disabled (Model.Cursor.atMax markets)
                , Button.attrs
                    [ onClick IncrementMarketCursor ]
                ]
                [ span [ class "fa fa-arrow-right" ] [] ]
            ]
        |> Accordion.prependHeader
            [ Button.button
                [ Button.primary
                , Button.disabled (Model.Cursor.atMin markets)
                , Button.attrs
                    [ onClick DecrementMarketCursor ]
                ]
                [ span [ class "fa fa-arrow-left" ] [] ]
            ]


viewForm : Markets -> List (Block.Item Msg)
viewForm markets =
    let
        currentBook =
            Model.Cursor.current markets |> Maybe.map .book
    in
    [ currentBook
        |> Maybe.andThen OrderBook.bestBuy
        |> Maybe.withDefault zeroOrder
        |> viewOrder "Buy At"
    , currentBook
        |> Maybe.andThen OrderBook.bestSell
        |> Maybe.withDefault zeroOrder
        |> viewOrder "Sell At"
    ]
        |> List.concat


viewOrder : String -> Order -> List (Block.Item Msg)
viewOrder title order =
    [ Block.titleH5 [] [ text title ]
    , Block.custom <|
        Form.form []
            [ Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.xs3, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Price" ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.plainText True, Input.value <| String.fromInt order.price ] ]
                , Form.colLabel [ Col.xs3, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Quantity" ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.plainText True, Input.value <| String.fromInt order.quantity ] ]
                ]
            ]
    ]


currentMarketName : Markets -> String
currentMarketName =
    Model.Cursor.current
        >> Maybe.map .name
        >> Maybe.withDefault "None"


displayTitle : String -> String
displayTitle title =
    title ++ " Market" |> String.Extra.toTitleCase


zeroOrder : Order
zeroOrder =
    Order 0 0 0
