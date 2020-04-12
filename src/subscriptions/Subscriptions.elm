module Subscriptions exposing (subscriptions)

import Bootstrap.Accordion as Accordion
import Model exposing (Model)
import Model.Clock
import Update exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Model.Clock.subscriptions model.clock Tick
        , Accordion.subscriptions model.accordionState AccordionMsg
        ]
