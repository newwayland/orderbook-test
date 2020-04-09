module View.Random exposing (view)

import Html exposing (Attribute, Html, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json
import Model.Random exposing (Seed)
import Update exposing (Msg(..))


view : Seed -> Html Msg
view seed =
    div []
        [ input
            [ placeholder "Model Number", value (String.fromInt seed.display), onInput ChangeSeed, onEnter ResetModel ]
            []
        ]


{-| When the enter key is released, send the `msg`.
Otherwise, do nothing.
-}
onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.succeed onEnterAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode
