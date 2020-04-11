module View.Extra exposing (onEnter)

import Html exposing (Attribute, Html, text)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json


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
