module View.Individual exposing (view, viewCursor)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Model.Individual exposing (Individuals, Sex(..))
import Model.Types exposing (BirthDate)
import Update exposing (Msg(..))


view : Individuals -> (BirthDate -> String) -> (BirthDate -> String) -> Html Msg
view individuals displayBirthDate displayAge =
    Card.config [ Card.align Text.alignXsLeft ]
        |> Card.block []
            [ viewCursor individuals
            , viewForm
                individuals
                displayBirthDate
                displayAge
            ]
        |> Card.view


viewForm : Individuals -> (BirthDate -> String) -> (BirthDate -> String) -> Block.Item Msg
viewForm individuals displayBirthDate displayAge =
    let
        currentIndividual =
            Model.Individual.current individuals
    in
    Block.custom <|
        Form.form []
            [ Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Name" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| currentIndividual.name ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Sex" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displaySex currentIndividual.sex ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Born" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displayBirthDate currentIndividual.birthdate ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Age" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displayAge currentIndividual.birthdate ] ]
                ]
            ]


viewCursor : Individuals -> Block.Item Msg
viewCursor inds =
    let
        displayCursor =
            inds.current |> String.fromInt
    in
    Block.custom <|
        (InputGroup.config
            (InputGroup.number
                [ Input.placeholder "Individual Id"
                , Input.value displayCursor
                , Input.onInput ChangeCursor
                ]
            )
            |> InputGroup.predecessors
                [ InputGroup.button
                    [ Button.primary
                    , Button.disabled (Model.Individual.atMin inds)
                    , Button.attrs
                        [ onClick DecrementCursor ]
                    ]
                    [ span [ class "fa fa-arrow-left" ] [] ]
                ]
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.primary
                    , Button.disabled (Model.Individual.atMax inds)
                    , Button.attrs
                        [ onClick IncrementCursor ]
                    ]
                    [ span [ class "fa fa-arrow-right" ] [] ]
                , InputGroup.button
                    [ Button.primary, Button.attrs [ onClick RandomCursor ] ]
                    [ text "Random" ]
                ]
            |> InputGroup.view
        )


displaySex : Model.Individual.Sex -> String
displaySex sex =
    case sex of
        Male ->
            "Male"

        Female ->
            "Female"
