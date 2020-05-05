module View exposing (view)

import Bootstrap.Accordion as Accordion
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Text as Text
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, name, style)
import Model exposing (Model)
import Update exposing (Msg)
import View.Clock exposing (Clock)
import View.Individual
import View.Market
import View.Polity exposing (Polity)
import View.Random exposing (Seed)



--import View.World


view : Model -> Html Msg
view model =
    div [ style "max-width" "991px" ]
        [ viewPortHintWorkaround
        , CDN.stylesheet
        , CDN.fontAwesome
        , Accordion.config Update.AccordionMsg
            |> Accordion.withAnimation
            |> Accordion.cards
                [ worldCard "card1" model
                , View.Individual.card "card2" model.individuals (View.Clock.displayBirthDate model.clock) (View.Clock.displayAge model.clock)
                , View.Market.card "card3" model.markets
                ]
            |> Accordion.view model.accordionState
        ]



{- Set the viewport hint properly to keep iPhones happy -}


viewPortHintWorkaround : Html msg
viewPortHintWorkaround =
    node "meta"
        [ name "viewport", attribute "content" "width=device-width, initial-scale=1, shrink-to-fit=no" ]
        []


type alias World a =
    { a
        | clock : Clock
        , seed : Seed
        , polity : Polity
    }


worldCard : String -> World a -> Accordion.Card Msg
worldCard seq world =
    Accordion.card
        { id = seq
        , options = [ Card.align Text.alignXsCenter ]
        , header = View.Clock.clockCardHeader world.clock
        , blocks =
            [ Accordion.block [ Block.align Text.alignXsLeft ]
                [ View.Clock.clockControlsBlock
                ]
            , Accordion.block []
                [ View.Random.viewSeedId world.seed ]
            , Accordion.block []
                [ View.Polity.viewAgeCategories world.polity ]
            ]
        }
