module Pages.PlayAlong exposing (Chord, Model, Msg, initModel, initMsg, page, subscriptions, update)

import Api.Api exposing (ApiChord(..), loadChords)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Layout.Helper as LH exposing (..)
import Svg exposing (Svg)
import Task
import Time


type Msg
    = Init
    | Start
    | Stop
    | Tick
    | Loaded (Result String (List (ApiChord Msg)))
    | UpdateBpm Int


type Model
    = UnInitialized (List Int)
    | LoadError String
    | PlayAlong PlayInfo


type alias PlayInfo =
    { before : List (Chord Msg)
    , current : Chord Msg
    , after : List (Chord Msg)
    , state : State
    , bpm : Int
    , barLength : Int
    }


type State
    = Playing
    | Stopped


type alias Chord msg =
    { id : Int, name : String, svg : Svg msg, svgHeight : Int, svgWidth : Int }


initModel : List Int -> List (Chord a) -> Model
initModel ids chords =
    case chords of
        [] ->
            UnInitialized ids

        c :: cs ->
            PlayAlong { current = mapChordMsg c, before = [], after = List.map mapChordMsg cs, state = Playing, bpm = defaultBpm, barLength = defaultBarLength }


defaultBarLength : Int
defaultBarLength =
    4


defaultBpm : Int
defaultBpm =
    70


initMsg : Cmd Msg
initMsg =
    Task.perform (\a -> a) (Task.succeed Init)



--TODO call


mapChordMsg : Chord a -> Chord Msg
mapChordMsg chord =
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
    let
        body =
            case model of
                PlayAlong p ->
                    column []
                        [ viewChords p
                        , viewControls p
                        ]

                _ ->
                    Element.none
    in
    column [ width fill, padding 10 ]
        [ LH.header "Play along"
        , LH.spacerLine
        , body
        ]



-- UPDATE


mapPlayInfo : (PlayInfo -> PlayInfo) -> Model -> Model
mapPlayInfo f model =
    case model of
        PlayAlong p ->
            PlayAlong (f p)

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            ( mapPlayInfo (\info -> { info | state = Stopped }) model, Cmd.none )

        Start ->
            ( mapPlayInfo (\info -> { info | state = Playing }) model, Cmd.none )

        Init ->
            case model of
                UnInitialized (id :: ids) ->
                    ( model, loadChords (id :: ids) Loaded )

                _ ->
                    -- TODO redirect to /overview  OR show a message with link to overview
                    ( model, Cmd.none )

        Loaded apiChords ->
            ( mapChords apiChords, Cmd.none )

        Tick ->
            let
                d =
                    Debug.log "Tick" 2
            in
            ( mapPlayInfo setNextChord model, Cmd.none )

        UpdateBpm bpm ->
            ( mapPlayInfo (\info -> { info | bpm = bpm }) model, Cmd.none )



-- TODO stop ticking when no chords loaded


mapChords : Result String (List (ApiChord Msg)) -> Model
mapChords res =
    case res of
        Err err ->
            LoadError err

        Ok apiChords ->
            case List.map mapChord apiChords of
                [] ->
                    LoadError "No chords loaded"

                c :: cs ->
                    PlayAlong { before = [], current = c, after = cs, state = Stopped, bpm = defaultBpm, barLength = defaultBarLength }


mapChord : ApiChord Msg -> Chord Msg
mapChord (ApiChord c) =
    { id = c.id, name = c.name, svg = c.svg.svg, svgHeight = round c.svg.height, svgWidth = round c.svg.width }


setNextChord : PlayInfo -> PlayInfo
setNextChord ({ before, current, after } as info) =
    case after of
        next :: rest ->
            let
                bf =
                    before ++ [ current ]

                cur =
                    next
            in
            { info | before = bf, current = cur, after = rest }

        [] ->
            case before of
                [] ->
                    info

                b :: rest ->
                    { info | before = [], current = b, after = rest ++ [ current ] }



-- SUBSCRIPTIONS


bpmToTick : PlayInfo -> Float
bpmToTick info =
    let
        min =
            toFloat 1000 * 60

        beat =
            min / toFloat info.bpm
    in
    beat


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        PlayAlong info ->
            case info.state of
                Playing ->
                    Time.every (bpmToTick info) (\_ -> Tick)

                Stopped ->
                    Sub.none

        _ ->
            Sub.none



-- VIEW


viewControls : PlayInfo -> Element Msg
viewControls info =
    let
        startStop =
            case info.state of
                Playing ->
                    stopControl info

                Stopped ->
                    startControl info
    in
    Element.row [ padding 10, spacing 10 ] [ startStop, tempoControl info ]


stopControl : PlayInfo -> Element Msg
stopControl info =
    Input.button [ Background.color stopColor ] { label = text "Stop", onPress = Just Stop }


startControl : PlayInfo -> Element Msg
startControl info =
    Input.button [ Background.color startColor ] { label = text "Start", onPress = Just Start }


tempoControl : PlayInfo -> Element Msg
tempoControl info =
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color LH.gray
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = round >> UpdateBpm
        , label =
            Input.labelAbove []
                (text ("Bpm: " ++ String.fromInt info.bpm))
        , min = 60
        , max = 200
        , step = Just 1
        , value = toFloat info.bpm
        , thumb =
            Input.defaultThumb
        }


viewChords : PlayInfo -> Element Msg
viewChords info =
    let
        before =
            List.map (viewChord LH.white) info.before

        current =
            viewChord playColor info.current

        after =
            List.map (viewChord LH.white) info.after
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


startColor : Color
startColor =
    Element.rgb255 100 250 140


stopColor : Color
stopColor =
    Element.rgb255 255 10 10
