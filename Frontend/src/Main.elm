module Main exposing (..)

--import Pages.CreateChord exposing (Model)

import Browser
import Browser.Events as E
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Decoders exposing (..)
import Html exposing (..)
import Http
import Routing
import Url exposing (Url)



-- MAIN


main =
    Browser.application
        { init = init
        , update = Routing.update
        , view = Routing.view
        , subscriptions = subscriptions
        , onUrlChange = Routing.onUrlChange
        , onUrlRequest = Routing.onUrlRequest
        }


init : () -> Url -> Nav.Key -> ( Routing.Model, Cmd Routing.Msg )
init _ url key =
    Routing.init url key



-- UPDATE
-- SUBSCRIPTIONS


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
