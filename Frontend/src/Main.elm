module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Routing
import Url exposing (Url)



-- MAIN


main =
    Browser.application
        { init = init
        , update = Routing.update
        , view = Routing.view
        , subscriptions = Routing.subscriptions
        , onUrlChange = Routing.onUrlChange
        , onUrlRequest = Routing.onUrlRequest
        }


init : () -> Url -> Nav.Key -> ( Routing.Model, Cmd Routing.Msg )
init _ url key =
    Routing.init url key
