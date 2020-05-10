module View.Polity exposing
    ( Polity
    , viewAgeCategories
    , viewPopulation
    )

import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Html exposing (div, label, text)
import Html.Attributes exposing (class, for, placeholder, selected, style, value)
import Html.Events exposing (onInput)
import Model.Polity
import Update exposing (Msg(..))
import View.Extra exposing (onEnter)


{-| Re-expose the type through the view so the Main view doesn't need to see the model
-}
type alias Polity =
    Model.Polity.Polity


type alias SelectLimits =
    { min : Int
    , max : Int
    , current : Int
    }


viewPopulation : Int -> Block.Item Msg
viewPopulation popn =
    let
        displayPopn =
            String.fromInt popn
    in
    InputGroup.config
        (InputGroup.number
            [ Input.placeholder "Population"
            , Input.value displayPopn
            , Input.onInput ChangePopulation
            , Input.attrs [ style "max-width" "11.25em", onEnter ResetModel ]
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [ style "min-width" "5em" ] [ text "Population" ] ]
        |> InputGroup.view
        |> Block.custom


{-| Display and update age categories
-}
viewAgeCategories : Polity -> Block.Item Msg
viewAgeCategories settings =
    Block.custom <|
        Form.formInline []
            [ SelectLimits
                0
                (Model.Polity.majority settings - 1)
                (Model.Polity.schoolAge settings)
                |> ageSelect
                    "School"
                    ChangeSchoolAge
            , SelectLimits
                (Model.Polity.schoolAge settings + 1)
                (Model.Polity.retirementAge settings - 1)
                (Model.Polity.majority settings)
                |> ageSelect
                    "Adult"
                    ChangeWorkingAge
            , SelectLimits
                (Model.Polity.majority settings + 1)
                120
                (Model.Polity.retirementAge settings)
                |> ageSelect
                    "Retire"
                    ChangeRetirementAge
            ]


ageSelect : String -> (String -> Msg) -> SelectLimits -> Html.Html Msg
ageSelect id msg ages =
    div [ class "input-group mb-3" ]
        [ div [ class "input-group-prepend" ]
            [ label [ class "input-group-text", for id, style "min-width" "5em" ] [ text id ] ]
        , generateRange ages
            |> Select.custom
                [ Select.id id
                , Select.onChange msg
                ]
        ]


generateRange : SelectLimits -> List (Select.Item msg)
generateRange ages =
    List.range ages.min ages.max
        |> List.map (\x -> generateItem x (x == ages.current))


generateItem : Int -> Bool -> Select.Item msg
generateItem element isSelected =
    let
        elementStr =
            String.fromInt element
    in
    Select.item [ value elementStr, selected isSelected ] [ text elementStr ]
