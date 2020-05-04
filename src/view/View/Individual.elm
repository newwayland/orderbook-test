module View.Individual exposing (card)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (span, text)
import Html.Attributes exposing (class, readonly, style)
import Html.Events exposing (onClick, onInput)
import Model.Cursor
import Model.Individual exposing (Individual, Sex(..))
import Model.Individuals exposing (Individuals)
import Model.Types exposing (BirthDate)
import Update exposing (Msg(..))


card : String -> Individuals -> (BirthDate -> String) -> (BirthDate -> String) -> Accordion.Card Msg
card seq individuals displayBirthDate displayAge =
    Accordion.card
        { id = seq
        , options = [ Card.align Text.alignXsCenter ]
        , header = individualCardHeader individuals
        , blocks =
            [ Accordion.block [ Block.align Text.alignXsLeft ]
                [ viewCursor individuals
                , viewForm
                    individuals
                    displayBirthDate
                    displayAge
                ]
            ]
        }


individualCardHeader : Individuals -> Accordion.Header Msg
individualCardHeader inds =
    Accordion.toggle [ style "min-width" "75%" ]
        [ Button.button
            [ Button.outlinePrimary
            , Button.large
            , Button.block
            ]
            [ currentIndividual inds |> Model.Individual.name |> text ]
        ]
        |> Accordion.header []
        |> Accordion.appendHeader
            [ Button.button
                [ Button.primary
                , Button.disabled (Model.Cursor.atMax inds)
                , Button.attrs
                    [ onClick IncrementIndividualCursor ]
                ]
                [ span [ class "fa fa-arrow-right" ] [] ]
            ]
        |> Accordion.prependHeader
            [ Button.button
                [ Button.primary
                , Button.disabled (Model.Cursor.atMin inds)
                , Button.attrs
                    [ onClick DecrementIndividualCursor ]
                ]
                [ span [ class "fa fa-arrow-left" ] [] ]
            ]


viewForm : Individuals -> (BirthDate -> String) -> (BirthDate -> String) -> Block.Item Msg
viewForm individuals displayBirthDate displayAge =
    let
        currentInd =
            currentIndividual individuals

        birthdate =
            currentInd |> Model.Individual.birthDate
    in
    Block.custom <|
        Form.form []
            [ Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Sex" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displaySex currentInd ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Born" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displayBirthDate birthdate ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Age" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displayAge birthdate ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Cash" ]
                , Form.col []
                    [ Input.text [ Input.plainText True, Input.value <| displayCash currentInd ] ]
                ]
            , Form.row [ Row.attrs [ Spacing.m0 ] ]
                [ Form.colLabel [ Col.sm2, Col.attrs [ Spacing.pl0, class "text-muted" ] ] [ text "Diary" ]
                , Form.col []
                    [ Textarea.textarea [ Textarea.rows 4, Textarea.attrs [ readonly True ], Textarea.value <| displayDiary currentInd ] ]
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
                , Input.onInput ChangeIndividualCursor
                , Input.attrs [ style "max-width" "11.25em" ]
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.primary, Button.attrs [ onClick RandomIndividualCursor ] ]
                    [ text "Random" ]
                ]
            |> InputGroup.view
        )


displaySex : Individual -> String
displaySex ind =
    case Model.Individual.sex ind of
        Male ->
            "Male"

        Female ->
            "Female"


displayCash : Individual -> String
displayCash =
    Model.Individual.cash >> String.fromInt


displayDiary : Individual -> String
displayDiary =
    Model.Individual.journal >> String.join "\n"


currentIndividual : Individuals -> Individual
currentIndividual inds =
    Model.Cursor.current inds |> Maybe.withDefault Model.Individual.defaultIndividual
