module Pages.PlayAlong exposing (Model, Msg, initModel, initMsg, page, subscriptions, update)

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


type Msg
    = Start
    | Stop
    | Tick
    | UpdateBpm Int
    | Edit
    | FinishEdit
    | KeyPressed KeyboardAction
    | KeyDown KeyboardAction
    | KeyUp KeyboardAction
    | EditButton Strumming.EditAction


type KeyboardAction
    = EditKeyPress Strumming.EditAction
    | PlayingKeyPress PlayingAction


type PlayingAction
    = StartEdit
    | StartStop
    | None


type alias Model =
    { state : State
    , strumming : Strumming.Model
    }


type State
    = Playing
    | Stopped
    | Editing


initModel : Model
initModel =
    { state = defaultState
    , strumming = Strumming.initModel
    }


defaultState : State
defaultState =
    Stopped


initMsg : Cmd Msg
initMsg =
    Cmd.none


page : Model -> Element Msg
page model =
    let
        body =
            viewPlayAlong model
    in
    column [ width fill, padding 10 ]
        [ --Font awesome style
          Element.html FontAwesome.Styles.css
        , LH.header "Play along"
        , LH.spacerLine
        , body
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            ( { model | state = Stopped }, Cmd.none )

        Start ->
            ( { model | state = Playing }, Cmd.none )

        Tick ->
            updateBeat model

        UpdateBpm bpm ->
            ( { model | strumming = Strumming.updateBpm model.strumming bpm }, Cmd.none )

        Edit ->
            ( { model | state = Editing, strumming = Strumming.setEdit model.strumming }, Cmd.none )

        FinishEdit ->
            ( { model | state = Stopped, strumming = Strumming.finishEdit model.strumming }, Cmd.none )

        KeyPressed action ->
            handleKeyboard model action

        KeyDown action ->
            handleKeyboard model action

        KeyUp action ->
            handleKeyboard model action

        EditButton action ->
            ( { model | strumming = Strumming.updateAndAdvance model.strumming action }, Cmd.none )


handleKeyboard : Model -> KeyboardAction -> ( Model, Cmd msg )
handleKeyboard model action =
    let
        d =
            Debug.log "model, action" ( model, action )
    in
    case action of
        EditKeyPress a ->
            ( { model | strumming = Strumming.updateAndAdvance model.strumming a }, Cmd.none )

        PlayingKeyPress a ->
            case a of
                StartEdit ->
                    ( { model | state = Editing }, Cmd.none )

                None ->
                    ( model, Cmd.none )

                StartStop ->
                    ( model, Cmd.none )


updateBeat : Model -> ( Model, Cmd msg )
updateBeat info =
    let
        ( strumming, changed, cmd ) =
            Strumming.tick info.strumming
    in
    ( { info | strumming = strumming }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions info =
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


keyDecoder : Model -> Decode.Decoder Msg
keyDecoder info =
    case info.state of
        Playing ->
            keyboardPlayStop

        Editing ->
            keyboardEditing

        Stopped ->
            keyboardPlayStop


keyboardEditing : Decode.Decoder Msg
keyboardEditing =
    Decode.oneOf
        [ Decode.map (\x -> KeyPressed (EditKeyPress x)) editControlsDecoder
        ]


editControlsDecoder : Decode.Decoder Strumming.EditAction
editControlsDecoder =
    Decode.map Strumming.getEditAction (Decode.field "key" Decode.string)


keyboardPlayStop : Decode.Decoder Msg
keyboardPlayStop =
    Decode.map (\x -> KeyPressed (PlayingKeyPress x)) (Decode.map toPlayAction (Decode.field "key" Decode.string))


toPlayAction : String -> PlayingAction
toPlayAction string =
    let
        d =
            Debug.log "keyString " string
    in
    case string of
        "spacebar" ->
            StartStop

        "e" ->
            StartEdit

        _ ->
            None



-- VIEW


viewPlayAlong : Model -> Element Msg
viewPlayAlong info =
    column [ spacing 5 ]
        [ viewControls info
        , SR.view [ centerX ] info.strumming
        , viewEditControls info
        ]


viewEditControls : Model -> Element Msg
viewEditControls info =
    let
        editFinish =
            viewEditFinish info

        editType =
            viewEditType info

        inEditor =
            if info.state == Editing then
                editControls info

            else
                Element.none
    in
    Element.row [ spacing 5 ] [ editFinish, inEditor ]


editControls : Model -> Element Msg
editControls model =
    Element.row [ spacing 5 ] (List.map viewControl (Dict.toList Strumming.controls))


viewControl : ( String, Strumming.Control ) -> Element Msg
viewControl ( key, control ) =
    case control.description of
        Nothing ->
            Element.none

        Just d ->
            button (Just (KeyPressed (EditKeyPress control.action))) (Element.text d)


viewEditFinish : Model -> Element Msg
viewEditFinish info =
    case info.state of
        Editing ->
            button (Just FinishEdit) (Element.text "Finish")

        _ ->
            button (Just Edit) (Element.text "Edit")


viewEditType : Model -> Element Msg
viewEditType { strumming, state } =
    Element.text <| Strumming.editStateLabel strumming


viewControls : Model -> Element Msg
viewControls info =
    let
        startStop =
            startStopControl info
    in
    Element.row [ centerX, padding 10, spacing 10 ] [ startStop, tempoControl info ]


startStopControl : Model -> Element Msg
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


tempoControl : Model -> Element Msg
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
                (text ("Bpm: " ++ String.fromInt info.strumming.pattern.bpm))
        , min = 60
        , max = 200
        , step = Just 1
        , value = toFloat info.strumming.pattern.bpm
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


buttonBackground : Color
buttonBackground =
    Element.rgb255 200 200 200



-- HELPER


button : Maybe Msg -> Element Msg -> Element Msg
button onPress label =
    Input.button [ spacing 5, padding 5, Border.rounded 10, Background.color buttonBackground ] { label = label, onPress = onPress }
