module View.Random exposing (view)

import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json
import Model.Random exposing (Seed)
import Update exposing (Msg(..))


view : Seed -> Html Msg
view seed =
    div []
        [ InputGroup.config
            (InputGroup.text
                [ Input.placeholder "Model Number"
                , Input.value (String.fromInt seed.display)
                , Input.onInput ChangeSeed
                , Input.attrs [ onEnter ResetModel ]
                ]
            )
            |> InputGroup.predecessors
                [ InputGroup.span [] [ text "seed" ] ]
            |> InputGroup.view
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
