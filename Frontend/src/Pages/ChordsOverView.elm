module Pages.ChordsOverview exposing (Model, Msg, initModel, initMsg, page, update)

import Api.Api exposing (ApiChord(..), loadChords)
import Element exposing (..)
import Element.Input
import Layout.Helper as LH exposing (..)
import Svg exposing (Svg)



--TYPES


type alias Model =
    Result String (List Chord)


type alias Chord =
    { id : Int, name : String, svg : Svg Msg, tags : List String }


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
        [ LH.header
        , LH.spacerLine
        , viewModel model
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        d =
            Debug.log "Update Overview" msg
    in
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
    { id = chord.id, name = chord.name, svg = chord.svg, tags = chord.tags }



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
    wrappedRow [] (List.map viewChord chords)


viewChord : Chord -> Element Msg
viewChord c =
    column [ height (px 400), width (px 400) ] [ html c.svg, row [ centerX ] [ text c.name, viewTags c.tags ] ]


viewTags : List String -> Element Msg
viewTags tags =
    row [ centerX, spacing 10, padding 10 ] (List.map viewTag tags)


viewTag : String -> Element Msg
viewTag tag =
    el [] (text tag)
