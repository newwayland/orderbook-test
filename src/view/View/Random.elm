module View.Random exposing (view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
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
    Card.config []
        |> Card.block []
            [ Block.custom <| seedInputGroup (String.fromInt seed.display) ]
        |> Card.view


seedInputGroup : String -> Html Msg
seedInputGroup displaySeed =
    InputGroup.config
        (InputGroup.text
            [ Input.placeholder "Model Number"
            , Input.value displaySeed
            , Input.onInput ChangeSeed
            , Input.attrs [ onEnter ResetModel ]
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text "Model #" ] ]
        |> InputGroup.view


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
