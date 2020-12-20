module Layout exposing (viewMain)

import ChordChart exposing (..)
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
            , viewCreateButton
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

        CreatingChord ->
            createChordView

        None ->
            createChordView


frontPage : Element Msg
frontPage =
    row [ width fill ]
        [ column [ height fill, width <| fillPortion 5 ] [ button (LH.buttonLayout ++ [ height fill, centerX ]) { label = text "Browse chords", onPress = Just LoadingChords } ]
        , column [ height fill, width <| fillPortion 5 ] [ viewCreateButton ]
        ]


chordsView : List Chord -> Element Msg
chordsView chords =
    wrappedRow [] (List.map viewChord chords)


viewCreateButton : Element Msg
viewCreateButton =
    button (LH.buttonLayout ++ [ centerX ]) { label = text "Create chords", onPress = Just CreateChord }


viewChord : Chord -> Element Msg
viewChord c =
    column [ height (px 400), width (px 400) ] [ html c.svg, row [ centerX ] [ text c.name, viewTags c.tags ] ]


viewTags : List String -> Element Msg
viewTags tags =
    row [ centerX, spacing 10, padding 10 ] (List.map viewTag tags)


viewTag : String -> Element Msg
viewTag tag =
    el [] (text tag)
