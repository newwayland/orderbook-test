module View.Polity exposing (Polity, viewAgeCategories)

import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Html exposing (div, label, text)
import Html.Attributes exposing (class, for, placeholder, selected, style, value)
import Html.Events exposing (onInput)
import Model.Polity
import Update exposing (Msg(..))


{-| Re-expose the type through the view so the Main view doesn't need to see the model
-}
type alias Polity =
    Model.Polity.Polity


{-| Display and update age categories
-}
viewAgeCategories : Polity -> Block.Item Msg
viewAgeCategories settings =
    Block.custom <|
        Form.formInline []
            [ div [ class "input-group mb-3" ]
                [ div [ class "input-group-prepend" ]
                    [ label [ class "input-group-text", for "schoolAge", style "min-width" "5em" ] [ text "School" ] ]
                , generateRange
                    0
                    (Model.Polity.majority settings - 1)
                    (Model.Polity.schoolAge settings)
                    |> Select.custom
                        [ Select.id "schoolAge"
                        , Select.onChange ChangeSchoolAge
                        ]
                ]
            , div [ class "input-group mb-3" ]
                [ div [ class "input-group-prepend" ]
                    [ label [ class "input-group-text", for "workingAge", style "min-width" "5em" ] [ text "Adult" ] ]
                , generateRange
                    (Model.Polity.schoolAge settings + 1)
                    (Model.Polity.retirementAge settings - 1)
                    (Model.Polity.majority settings)
                    |> Select.custom
                        [ Select.id "workingAge"
                        , Select.onChange ChangeWorkingAge
                        ]
                ]
            , div [ class "input-group mb-3" ]
                [ div [ class "input-group-prepend" ]
                    [ label [ class "input-group-text", for "retirementAge", style "min-width" "5em" ] [ text "Retire" ] ]
                , generateRange
                    (Model.Polity.majority settings + 1)
                    120
                    (Model.Polity.retirementAge settings)
                    |> Select.custom
                        [ Select.id "workingAge"
                        , Select.onChange ChangeRetirementAge
                        ]
                ]
            ]


generateRange : Int -> Int -> Int -> List (Select.Item msg)
generateRange min max current =
    List.range min max
        |> List.map (\x -> generateItem x (x == current))


generateItem : Int -> Bool -> Select.Item msg
generateItem element isSelected =
    let
        elementStr =
            String.fromInt element
    in
    Select.item [ value elementStr, selected isSelected ] [ text elementStr ]
