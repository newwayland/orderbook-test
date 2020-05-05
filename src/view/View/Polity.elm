module View.Polity exposing (Polity, viewAgeCategories)

import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Html exposing (text)
import Html.Attributes exposing (placeholder, selected, value)
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
            [ generateRange
                0
                (Model.Polity.majority settings - 1)
                (Model.Polity.schoolAge settings)
                |> Select.select
                    [ Select.id "schoolAge"
                    , Select.small
                    , Select.onChange ChangeSchoolAge
                    ]
            , generateRange
                (Model.Polity.schoolAge settings + 1)
                (Model.Polity.retirementAge settings - 1)
                (Model.Polity.majority settings)
                |> Select.select
                    [ Select.id "workingAge"
                    , Select.small
                    , Select.onChange ChangeWorkingAge
                    ]
            , generateRange
                (Model.Polity.majority settings + 1)
                120
                (Model.Polity.retirementAge settings)
                |> Select.select
                    [ Select.id "workingAge"
                    , Select.small
                    , Select.onChange ChangeRetirementAge
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
