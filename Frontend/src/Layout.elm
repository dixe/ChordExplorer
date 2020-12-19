module Layout exposing (viewMain)

import Types exposing (..)
import Html as Html exposing (Html, div,p, label, h3, map)
import Html.Attributes exposing (class, href)
import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font

import Debug exposing (todo)
import LayoutHelpers as LH


viewMain : Model -> Html Msg
viewMain model =
    Element.layout
        [ Font.size 20
        ]
    <|
        column [width fill, spacing 10]
            [
             LH.header
            , LH.spacerLine
            , text "What ever bab"
            ]
