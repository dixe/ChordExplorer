module Pages.PlayAlong exposing (Chord, Model, Msg, initModel, initMsg, page, update)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import Layout.Helper as LH exposing (..)
import Svg exposing (Svg)


type Msg
    = Start
    | Stop


type Model
    = UnInitialized (List Int)
    | PlayAlong
        { before : List (Chord Msg)
        , current : Chord Msg
        , after : List (Chord Msg)
        , state : State
        }


type State
    = Playing
    | Stopped


type alias Chord msg =
    { id : Int, name : String, svg : Svg msg, svgHeight : Int, svgWidth : Int }


initModel : List (Chord a) -> Model
initModel chords =
    case chords of
        [] ->
            UnInitialized []

        c :: cs ->
            PlayAlong { current = mapChord c, before = [], after = List.map mapChord cs, state = Stopped }


initMsg : Cmd Msg
initMsg =
    Cmd.none



--TODO call


mapChord : Chord a -> Chord Msg
mapChord chord =
    let
        newSvg : Svg Msg
        newSvg =
            Svg.map mapSvg chord.svg
    in
    { id = chord.id, name = chord.name, svg = newSvg, svgHeight = chord.svgHeight, svgWidth = chord.svgWidth }


mapSvg : a -> Msg
mapSvg _ =
    Stop


page : Model -> Element Msg
page model =
    column [ width fill, padding 10 ]
        [ LH.header "Play along"
        , LH.spacerLine
        , viewChords model
        , viewControls model
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            ( model, Cmd.none )

        Start ->
            ( model, Cmd.none )



-- VIEW


viewControls : Model -> Element Msg
viewControls model =
    case model of
        UnInitialized ids ->
            Element.none

        PlayAlong data ->
            case data.state of
                Playing ->
                    el [] (text "View play controls")

                Stopped ->
                    el [] (text "View stopped controls")


viewChords : Model -> Element Msg
viewChords model =
    case model of
        UnInitialized ids ->
            Element.none

        PlayAlong chords ->
            let
                before =
                    List.map (viewChord LH.white) chords.before

                current =
                    viewChord playColor chords.current

                after =
                    List.map (viewChord LH.white) chords.after
            in
            wrappedRow [ spacing 10, padding 10 ] (before ++ [ current ] ++ after)


viewChord : Color -> Chord Msg -> Element Msg
viewChord color c =
    let
        viewHeight =
            c.svgHeight

        viewWidth =
            c.svgWidth
    in
    --TODO maybe set height and width, so all elements will be the same
    column
        [ Border.solid, Border.width 1, Border.rounded 40, padding 2, Background.color color ]
        [ viewSvg c.svgHeight c.svg
        , viewName c.name
        ]


viewName : String -> Element Msg
viewName name =
    el [ Font.bold, paddingXY 30 5 ] (text name)


viewSvg : Int -> Svg Msg -> Element Msg
viewSvg minHeight svg =
    el [ height (fill |> minimum minHeight) ] (html svg)


playColor : Color
playColor =
    Element.rgb255 10 100 240
