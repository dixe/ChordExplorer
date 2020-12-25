module Pages.ChordsOverview exposing (Model, Msg, initModel, initMsg, page, update)

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
    { id : Int, name : String, svg : Svg Msg, tags : List String, svgHeight : Int, svgWidth : Int }


type Msg
    = LoadChords
    | ChordsLoaded (Result String (List (ApiChord Msg)))


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
    }



-- VIEW


viewModel : Model -> Element Msg
viewModel model =
    case model of
        Ok chords ->
            viewChords chords

        Err msg ->
            text msg


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
        ]


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
