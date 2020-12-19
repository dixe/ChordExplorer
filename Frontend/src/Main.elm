module Main exposing (..)

import Types exposing (..)
import Html exposing (..)
import Layout exposing ( viewMain)
import Browser
import Task


-- MAIN
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ = (Empty, Cmd.none)

-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PlaceHolder -> (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = viewMain model



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =  Sub.none
