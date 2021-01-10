module Pages.PlayAlong exposing (ChordBase, Model, Msg, initModel, initMsg, page, subscriptions, toChord, update)

import Api.Api exposing (ApiChord(..), loadChords)
import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Solid as IconSolid
import FontAwesome.Styles
import Json.Decode as Decode
import Layout.Helper as LH exposing (..)
import Svg exposing (Svg)
import SvgStrumming.StrummingRender as SR
import SvgStrumming.SvgStrumming as Strumming
import Task
import Time
import Utils.NonEmptyCyclicList as Cl
import Utils.Rythm as Rythm


type Msg
    = Init
    | Start
    | Stop
    | Tick
    | Loaded (Result String (List (ApiChord Msg)))
    | UpdateBpm Int
    | Edit
    | FinishEdit
    | KeyPressed KeyboardAction
    | KeyDown KeyboardAction
    | KeyUp KeyboardAction


type KeyboardAction
    = EditKeyPress Strumming.EditAction
    | PlayingKeyPress PlayingAction


type PlayingAction
    = StartEdit
    | StartStop


type Model
    = UnInitialized (List Int)
    | LoadError String
    | PlayAlong PlayInfo


type alias PlayInfo =
    { chords : Cl.NonEmptyCyclicList (Chord Msg)
    , state : State
    , strumming : Strumming.Model
    , rythm : Strumming.Rythm
    }


type State
    = Playing
    | Stopped
    | Editing


type alias ChordBase msg =
    { id : Int
    , name : String
    , svg : Svg msg
    , svgHeight : Int
    , svgWidth : Int
    }


type alias Chord msg =
    { id : Int
    , name : String
    , svg : Svg msg
    , svgHeight : Int
    , svgWidth : Int
    }


toChord : ChordBase msg -> Chord msg
toChord base =
    { id = base.id, name = base.name, svg = base.svg, svgHeight = base.svgHeight, svgWidth = base.svgWidth }


initModel : List Int -> List (Chord a) -> Model
initModel ids chords =
    case chords of
        [] ->
            UnInitialized ids

        c :: cs ->
            PlayAlong (newPlayAlong (mapChordMsg c) (List.map mapChordMsg cs))


newPlayAlong : Chord Msg -> List (Chord Msg) -> PlayInfo
newPlayAlong current rest =
    { chords = Cl.init current rest
    , state = defaultState
    , strumming = SR.initModel Rythm.defaultRythm
    , rythm = Rythm.defaultRythm
    }


defaultState : State
defaultState =
    Stopped


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
                    viewPlayAlong p

                _ ->
                    Element.none
    in
    column [ width fill, padding 10 ]
        [ --Font awesome style
          Element.html FontAwesome.Styles.css
        , LH.header "Play along"
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


mapPlayInfoCmd : (PlayInfo -> ( PlayInfo, Cmd msg )) -> Model -> ( Model, Cmd msg )
mapPlayInfoCmd f model =
    case model of
        PlayAlong p ->
            let
                ( info, cmd ) =
                    f p
            in
            ( PlayAlong info, cmd )

        _ ->
            ( model, Cmd.none )


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
            mapPlayInfoCmd updateBeat model

        UpdateBpm bpm ->
            ( mapPlayInfo (\info -> { info | rythm = Rythm.updateBpm info.rythm bpm }) model, Cmd.none )

        Edit ->
            ( mapPlayInfo (\info -> { info | state = Editing, strumming = Strumming.setEdit info.strumming }) model, Cmd.none )

        FinishEdit ->
            ( mapPlayInfo (\info -> { info | state = Stopped, strumming = Strumming.finishEdit info.strumming }) model, Cmd.none )

        KeyPressed action ->
            handelKeyboard model action

        --                ( mapPlayInfo (\info -> { info | strumming = Strumming.updateAndAdvance info.strumming note }) model, Cmd.none )
        KeyDown action ->
            handelKeyboard model action

        --( mapPlayInfo (\info -> { info | strumming = Strumming.updateCommandKey info.strumming note }) model, Cmd.none )
        KeyUp action ->
            handelKeyboard model action



--( mapPlayInfo (\info -> { info | strumming = Strumming.updateCommandKey info.strumming note }) model, Cmd.none )


handelKeyboard : Model -> KeyboardAction -> ( Model, Cmd msg )
handelKeyboard model action =
    case action of
        EditKeyPress a ->
            ( mapPlayInfo (\info -> { info | strumming = Strumming.updateAndAdvance info.strumming a }) model, Cmd.none )

        PlayingKeyPress a ->
            ( model, Cmd.none )



{-
   case model of
       PlayAlong info ->
           case action of
               EditKeyPress a ->
                   ( mapPlayInfo (\i -> { i | strumming = Strumming.updateAndAdvance info.strumming note }) model, Cmd.none )

       _ ->
           model

-}
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
                    PlayAlong (newPlayAlong (mapChordMsg c) (List.map mapChordMsg cs))


mapChord : ApiChord Msg -> Chord Msg
mapChord (ApiChord c) =
    { id = c.id, name = c.name, svg = c.svg.svg, svgHeight = round c.svg.height, svgWidth = round c.svg.width }


updateBeat : PlayInfo -> ( PlayInfo, Cmd msg )
updateBeat info =
    let
        ( rythm, changed, cmd ) =
            Rythm.tick info.rythm

        newInfo =
            if changed then
                { info | chords = Cl.next info.chords }

            else
                info
    in
    ( { newInfo | rythm = rythm }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        PlayAlong info ->
            let
                keyboardInputs =
                    Browser.Events.onKeyUp <| keyDecoder info
            in
            case info.state of
                Playing ->
                    -- Maybe put tickTime into model
                    Sub.batch
                        [ Time.every (Strumming.tickTime info.strumming) (\_ -> Tick)
                        , keyboardInputs
                        ]

                Stopped ->
                    keyboardInputs

                Editing ->
                    keyboardInputs

        _ ->
            Sub.none


keyDecoder : PlayInfo -> Decode.Decoder Msg
keyDecoder info =
    case info.state of
        Playing ->
            Decode.fail "No keyboardMappings for playing"

        Editing ->
            Decode.map (\x -> KeyPressed (EditKeyPress x)) Strumming.noteDecoderEditing

        Stopped ->
            Decode.fail "No keyboard mappings for stopped"



-- VIEW


viewPlayAlong : PlayInfo -> Element Msg
viewPlayAlong info =
    column []
        [ -- viewChords info
          viewControls info
        , SR.view [ centerX ] info.strumming
        , viewEditControls info
        ]


viewEditControls : PlayInfo -> Element Msg
viewEditControls info =
    let
        editFinish =
            viewEditFinish info

        editType =
            viewEditType info

        inEditor =
            if info.state == Editing then
                [ editType ]

            else
                []
    in
    Element.row [ spacing 5 ] ([ editFinish ] ++ inEditor)


viewEditFinish : PlayInfo -> Element Msg
viewEditFinish info =
    case info.state of
        Editing ->
            Element.row [ Element.paddingXY 0 10 ]
                [ Input.button [] { label = Element.text "Finish", onPress = Just FinishEdit }
                ]

        _ ->
            Element.row [ Element.paddingXY 0 10 ]
                [ Input.button [] { label = Element.text "Edit", onPress = Just Edit }
                ]


viewEditType : PlayInfo -> Element Msg
viewEditType { strumming, state } =
    Element.text <| Strumming.editStateLabel strumming


viewChords : PlayInfo -> Element Msg
viewChords info =
    let
        before =
            List.map (viewChord []) info.chords.before

        current =
            viewChord currentAttribs info.chords.current

        after =
            List.map (viewChord []) info.chords.after
    in
    wrappedRow [ spacing 10, padding 10 ] (before ++ [ current ] ++ after)


viewChord : List (Element.Attribute Msg) -> Chord Msg -> Element Msg
viewChord attribs c =
    let
        viewHeight =
            c.svgHeight

        viewWidth =
            c.svgWidth
    in
    --TODO maybe set height and width, so all elements will be the same
    column
        ([ Border.solid, Border.width 2, Border.rounded 40, padding 2 ]
            ++ attribs
        )
        [ viewSvg c.svgHeight c.svg
        ]


viewControls : PlayInfo -> Element Msg
viewControls info =
    let
        startStop =
            startStopControl info
    in
    Element.row [ centerX, padding 10, spacing 10 ] [ startStop, tempoControl info ]


startStopControl : PlayInfo -> Element Msg
startStopControl info =
    case info.state of
        Playing ->
            controlButton IconSolid.stop Stop

        Stopped ->
            controlButton IconSolid.play Start

        Editing ->
            Element.none


controlButton : Icon.Icon -> Msg -> Element Msg
controlButton icon msg =
    let
        iconElm =
            Element.html (Icon.viewIcon icon)
    in
    Input.button (controlButtonAttribs ++ [ Background.color startTopBackground ]) { label = iconElm, onPress = Just msg }


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
                (text ("Bpm: " ++ String.fromInt info.rythm.bpm))
        , min = 60
        , max = 200
        , step = Just 1
        , value = toFloat info.rythm.bpm
        , thumb =
            Input.defaultThumb
        }


controlButtonAttribs : List (Attribute Msg)
controlButtonAttribs =
    [ Font.size 30, padding 10, Border.rounded 100 ]


viewName : String -> Element Msg
viewName name =
    el [ Font.bold, paddingXY 30 5 ] (text name)


viewSvg : Int -> Svg Msg -> Element Msg
viewSvg minHeight svg =
    el [ height (fill |> minimum minHeight) ] (html svg)


currentAttribs : List (Element.Attribute Msg)
currentAttribs =
    [ Border.color playColor
    , Border.glow playColor 10
    ]


playColor : Color
playColor =
    Element.rgb255 10 100 240


startTopBackground : Color
startTopBackground =
    Element.rgb255 200 200 200
