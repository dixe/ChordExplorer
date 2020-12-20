module Layout exposing (viewMain)

import Debug exposing (todo)
import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import Html as Html exposing (Html, div, h3, label, map, p)
import Html.Attributes exposing (class, href)
import LayoutHelpers as LH
import List
import Types exposing (..)


viewMain : Model -> Html Msg
viewMain model =
    Element.layout
        [ Font.size 20
        ]
    <|
        column [ width fill, spacing 10 ]
            [ LH.header
            , LH.spacerLine
            , viewModel model
            ]


viewModel : Model -> Element Msg
viewModel model =
    case model.status of
        SuccessAll chords ->
            frontPage

        Failure err ->
            text err

        Loading ->
            text "Loading"

        LoadedChords ->
            chordsView model.chordList

        None ->
            frontPage


frontPage : Element Msg
frontPage =
    row [ width fill ]
        [ column [ height fill, width <| fillPortion 5 ] [ button (LH.buttonLayout ++ [ height fill, centerX ]) { label = text "Browse chords", onPress = Just LoadingChords } ]
        , column [ height fill, width <| fillPortion 5 ] [ button (LH.buttonLayout ++ [ centerX ]) { label = text "Create chords", onPress = Nothing } ]
        ]


chordsView : List Chord -> Element Msg
chordsView chords =
    wrappedRow [] (List.map viewChord chords)


viewChord : Chord -> Element Msg
viewChord c =
    column [] [ html c.svg, text c.name, viewTags c.tags ]


viewTags : List String -> Element Msg
viewTags tags =
    row [] (List.map text tags)
