module Main exposing (..)

import Browser
import Subscriptions
import Update
import View


main =
    Browser.element
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
