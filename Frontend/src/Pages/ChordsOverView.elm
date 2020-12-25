module Pages.ChordsOverview exposing (Chord, Model, Msg, initModel, initMsg, page, update)

import Api.Api exposing (ApiChord(..), loadChords)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Layout.Helper as LH exposing (..)
import Svg exposing (Svg)



--TYPES


type alias Model =
    Result String (List Chord)


type alias Chord =
    { id : Int, name : String, svg : Svg Msg, tags : List String, svgHeight : Int, svgWidth : Int, selected : Bool }


type Msg
    = LoadChords
    | ChordsLoaded (Result String (List (ApiChord Msg)))
    | SelectChord Int
    | StartPlayAlong (List Chord)


initMsg : Cmd Msg
initMsg =
    loadChords ChordsLoaded


initModel : Model
initModel =
    Err "Not loaded"


page : Model -> Element Msg
page model =
    column [ width fill, spacing 10 ]
        [ LH.header "Overview"
        , LH.spacerLine
        , viewModel model
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadChords ->
            ( model, loadChords ChordsLoaded )

        ChordsLoaded res ->
            case res of
                Ok chords ->
                    ( Ok (mapChords chords), Cmd.none )

                Err errMsg ->
                    ( Err errMsg, Cmd.none )

        SelectChord id ->
            case model of
                Err e ->
                    ( Err e, Cmd.none )

                Ok chords ->
                    ( Ok (updateSelected id chords), Cmd.none )

        StartPlayAlong chords ->
            ( model, Cmd.none )


updateSelected : Int -> List Chord -> List Chord
updateSelected id chords =
    case chords of
        [] ->
            []

        c :: cs ->
            let
                updated =
                    if c.id == id then
                        { c | selected = not c.selected }

                    else
                        c
            in
            updated :: updateSelected id cs


mapChords : List (ApiChord Msg) -> List Chord
mapChords apiChords =
    List.map mapChord apiChords


mapChord : ApiChord Msg -> Chord
mapChord (ApiChord chord) =
    { id = chord.id
    , name = chord.name
    , svg = chord.svg.svg
    , tags = chord.tags
    , svgHeight = round chord.svg.height
    , svgWidth = round chord.svg.width
    , selected = False
    }



-- VIEW


viewModel : Model -> Element Msg
viewModel model =
    case model of
        Ok chords ->
            column [ padding 10 ]
                [ viewPlayAlong chords
                , viewChords chords
                ]

        Err msg ->
            text msg


viewPlayAlong : List Chord -> Element Msg
viewPlayAlong chords =
    let
        selectedIds =
            List.map (\c -> c.id) <| List.filter (\c -> c.selected) chords
    in
    case selectedIds of
        [] ->
            Element.none

        ids ->
            let
                qString =
                    String.join "," <|
                        List.map String.fromInt ids

                url =
                    "/playAlong" ++ "?ids=" ++ qString
            in
            Element.link
                [ Background.color LH.green
                , padding 5
                , Font.size 30
                , Border.rounded 10
                ]
                { label = text "Start PlayAlong", url = url }


viewChords : List Chord -> Element Msg
viewChords chords =
    wrappedRow [ spacing 10, padding 10 ] (List.map viewChord chords)


viewChord : Chord -> Element Msg
viewChord c =
    let
        viewHeight =
            c.svgHeight

        viewWidth =
            c.svgWidth
    in
    --TODO maybe set height and width, so all elements will be the same
    column
        [ Border.solid, Border.width 1, Border.rounded 40, padding 2 ]
        [ viewSvg c.svgHeight c.svg
        , viewName c.name
        , viewTags viewWidth c.tags
        , viewSelect c.id c.selected
        ]


viewSelect : Int -> Bool -> Element Msg
viewSelect id selected =
    let
        color =
            if selected then
                LH.blue

            else
                LH.gray

        attribs =
            [ centerX, Background.color color, padding 3 ]
    in
    Element.Input.button attribs
        { onPress = Just (SelectChord id)
        , label = text "PlayAlong"
        }


viewSvg : Int -> Svg Msg -> Element Msg
viewSvg minHeight svg =
    el [ height (fill |> minimum minHeight) ] (html svg)


viewName : String -> Element Msg
viewName name =
    el [ Font.bold, paddingXY 30 5 ] (text name)


viewTags : Int -> List String -> Element Msg
viewTags maxWidth tags =
    wrappedRow [ width (fill |> maximum maxWidth), centerX, spacing 10, paddingXY 30 5 ] (List.map viewTag tags)


viewTag : String -> Element Msg
viewTag tag =
    el [ Font.size 19 ] (text tag)
