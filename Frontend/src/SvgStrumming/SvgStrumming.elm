port module SvgStrumming.SvgStrumming exposing (Duration(..), ImgInfo, KeyboardAction, Model, Note(..), Pattern, Pos, TimeSignature, editStateLabel, finishEdit, getBarWidth, getNoteDuration, getTotalBars, initModel, lineWidth, noteDecoderEditing, noteDecoderPlaying, noteWidth, setEdit, stemHeight, stemWidth, tick, tickTime, timeSigWidth, updateAndAdvance, updateBpm, updateCommandKey)

import Json.Decode as Decode
import Json.Encode as Encode
import Utils.NonEmptyCyclicList as Cl


port playPort : Encode.Value -> Cmd msg


type Note
    = Note Duration
    | Rest Duration


type Duration
    = Whole
    | Half
    | Quater
    | Eighth


type alias Pos =
    { x : Float, y : Float }


type alias ImgInfo =
    { imgHeight : Float
    , imgWidth : Float
    }


type Direction
    = Left
    | Right


type KeyboardAction
    = Edit EditAction
    | Playing PlayingAction


type PlayingAction
    = StartEdit
    | StartStop


type EditAction
    = Change Duration
    | Add
    | Move Direction
    | None
    | Delete
    | ToogleRest
    | SwitchRest


type alias TimeSignature =
    ( Int, Int )


type alias Pattern =
    { notes : Cl.NonEmptyCyclicList Note
    , timeSignature : TimeSignature
    , bpm : Int
    , noteTicks : Int
    , bpmTicks : Int
    }


type alias EditorState =
    { editType : Duration -> Note }


type alias Model =
    { info : ImgInfo, pattern : Pattern, editorState : EditorState }


initModel : Model
initModel =
    { info = createImgInfo defaultPattern
    , pattern = defaultPattern
    , editorState = initEditorState
    }


initEditorState : EditorState
initEditorState =
    { editType = Note }



-- LOGIC


editStateLabel : Model -> String
editStateLabel { editorState } =
    case editorState.editType Whole of
        Note _ ->
            "Note"

        Rest _ ->
            "Rest"


setEdit : Model -> Model
setEdit model =
    model


finishEdit : Model -> Model
finishEdit model =
    model


updateCommandKey : Model -> KeyboardAction -> Model
updateCommandKey ({ pattern, editorState } as model) action =
    case action of
        _ ->
            model



{-
   updateAndAdvandeEditing : Model -> EditAction -> Model
   updateAndAdvandeEditing ({ pattern, editorState } as model) action =
-}


updateAndAdvance : Model -> KeyboardAction -> Model
updateAndAdvance model action =
    case action of
        Edit edit ->
            updateAndAdvanceEdit model edit

        Playing play ->
            updateAndAdvancePlaying model play


updateAndAdvancePlaying : Model -> PlayingAction -> Model
updateAndAdvancePlaying model action =
    case action of
        StartStop ->
            model

        StartEdit ->
            model


updateAndAdvanceEdit : Model -> EditAction -> Model
updateAndAdvanceEdit ({ pattern, editorState } as model) action =
    case action of
        Change d ->
            { model | pattern = { pattern | notes = Cl.next <| Cl.updateCurrent (updateNoteDuration d) pattern.notes } }

        Add ->
            let
                newP =
                    { pattern | notes = Cl.advanceToNew (editorState.editType Quater) pattern.notes }

                newInfo =
                    createImgInfo newP
            in
            { model | pattern = newP, info = newInfo }

        Delete ->
            { model | pattern = { pattern | notes = Cl.delete pattern.notes } }

        Move d ->
            case d of
                Left ->
                    { model | pattern = { pattern | notes = Cl.left pattern.notes } }

                Right ->
                    { model | pattern = { pattern | notes = Cl.right pattern.notes } }

        ToogleRest ->
            let
                newState =
                    { editorState
                        | editType =
                            case editorState.editType Whole of
                                Note _ ->
                                    Rest

                                Rest _ ->
                                    Note
                    }
            in
            { model | editorState = newState }

        SwitchRest ->
            { model | pattern = { pattern | notes = Cl.updateCurrent switchNoteKind pattern.notes } }

        None ->
            model


switchNoteKind : Note -> Note
switchNoteKind note =
    case note of
        Note d ->
            Rest d

        Rest d ->
            Note d


updateNoteDuration : Duration -> Note -> Note
updateNoteDuration d note =
    case note of
        Note _ ->
            Note d

        Rest _ ->
            Rest d


tickTime : Model -> Float
tickTime { pattern } =
    let
        beatLength =
            beatDuration pattern.bpm

        -- find lowest note duration and calc tick time based on that
        lowest =
            lowestDuration Quater (Cl.getAll pattern.notes)

        minBeatValue =
            getDurationLength pattern.timeSignature lowest

        d =
            Debug.log "minBeatValue" ( minBeatValue, lowest )
    in
    beatLength * minBeatValue


beatDuration : Int -> Float
beatDuration bpm =
    let
        min =
            toFloat 1000 * 60
    in
    min / toFloat bpm


lowestDuration : Duration -> List Note -> Duration
lowestDuration init notes =
    case notes of
        [] ->
            init

        n :: ns ->
            let
                d =
                    getDuration n

                next =
                    pickLowest init d
            in
            lowestDuration next ns


pickLowest : Duration -> Duration -> Duration
pickLowest a b =
    case ( a, b ) of
        ( Whole, _ ) ->
            b

        ( _, Whole ) ->
            a

        ( Half, _ ) ->
            b

        ( _, Half ) ->
            a

        ( Quater, _ ) ->
            b

        ( _, Quater ) ->
            a

        ( Eighth, _ ) ->
            b


getDuration : Note -> Duration
getDuration note =
    case note of
        Note d ->
            d

        Rest d ->
            d


getTotalBeats : Pattern -> Int
getTotalBeats { timeSignature, notes } =
    let
        partial =
            getNoteDuration timeSignature

        total =
            round <| List.sum <| List.map partial <| Cl.getAll notes

        beatsPrBar =
            Tuple.first timeSignature

        rem =
            remainderBy beatsPrBar total
    in
    total + beatsPrBar - rem


createImgInfo : Pattern -> ImgInfo
createImgInfo pattern =
    let
        bars =
            toFloat <| getTotalBars pattern

        barsWidth =
            bars * getBarWidth pattern

        imgWidth =
            timeSigWidth + barsWidth

        imgHeight =
            100
    in
    { imgHeight = imgHeight
    , imgWidth = imgWidth
    }


defaultPattern1 : Pattern
defaultPattern1 =
    { notes = Cl.init (Note Eighth) [ Note Eighth, Note Quater, Note Whole, Note Half, Note Half, Note Quater ]
    , timeSignature = ( 4, 4 )
    , bpm = 70
    , noteTicks = 0
    , bpmTicks = 0
    }


defaultPattern : Pattern
defaultPattern =
    { notes = Cl.init (Note Eighth) [ Note Eighth, Note Quater, Note Quater, Note Whole, Note Half, Note Half ]
    , timeSignature = ( 4, 4 )
    , bpm = 70
    , noteTicks = 0
    , bpmTicks = 0
    }


updateBpm : Model -> Int -> Model
updateBpm ({ pattern } as model) bpm =
    let
        p =
            { pattern | bpm = bpm }
    in
    { model | pattern = p }


tick : Model -> ( Model, Bool, Cmd msg )
tick model =
    let
        ( updateNotes, movedNext ) =
            tickNotes model

        ( newModel, cmd ) =
            tickMetronome updateNotes
    in
    ( newModel, movedNext, cmd )


tickNotes : Model -> ( Model, Bool )
tickNotes ({ pattern } as model) =
    let
        time =
            tickTime model

        ticks =
            pattern.noteTicks + 1

        passed =
            toFloat ticks * time

        noteTime =
            noteTotalTime pattern

        moveNextNote =
            passed >= noteTime

        nextPattern =
            if moveNextNote then
                { pattern | notes = Cl.next pattern.notes }

            else
                pattern

        finalPattern =
            if moveNextNote then
                { nextPattern | noteTicks = 0 }

            else
                { pattern | noteTicks = ticks }
    in
    ( { model | pattern = finalPattern }, moveNextNote )


tickMetronome : Model -> ( Model, Cmd msg )
tickMetronome ({ pattern } as model) =
    let
        time =
            tickTime model

        ticks =
            pattern.bpmTicks + 1

        passed =
            toFloat ticks * time

        noteTime =
            getNoteDuration pattern.timeSignature (Note Quater)

        playNote =
            passed >= noteTime

        cmd =
            if playNote then
                playPort (Encode.bool playNote)

            else
                Cmd.none
    in
    ( model, cmd )


noteTotalTime : Pattern -> Float
noteTotalTime { bpm, timeSignature, notes } =
    let
        beatLen =
            beatDuration bpm

        noteBeats =
            getDurationLength timeSignature (getDuration <| Cl.cur notes)

        noteLen =
            noteBeats * beatLen
    in
    noteLen


getNoteDuration : TimeSignature -> Note -> Float
getNoteDuration timeSig note =
    case note of
        Note d ->
            getDurationLength timeSig d

        Rest d ->
            getDurationLength timeSig d


getDurationLength : TimeSignature -> Duration -> Float
getDurationLength ( top, bot ) dur =
    case dur of
        Whole ->
            toFloat top

        Half ->
            toFloat bot / 2

        Quater ->
            toFloat bot / 4

        Eighth ->
            toFloat bot / 8


getTotalBars : Pattern -> Int
getTotalBars ({ timeSignature, notes } as pattern) =
    getTotalBeats pattern // Tuple.first pattern.timeSignature


getBarWidth : Pattern -> Float
getBarWidth ({ timeSignature, notes } as pattern) =
    let
        beats =
            toFloat <| Tuple.first timeSignature
    in
    timeSigWidth + beats * 2 * noteWidth + lineWidth



-- SUBSCRIPTION DECODERS


noteDecoderPlaying : Decode.Decoder KeyboardAction
noteDecoderPlaying =
    Decode.map toEditAction (Decode.field "key" Decode.string)


noteDecoderEditing : Decode.Decoder KeyboardAction
noteDecoderEditing =
    Decode.map toEditAction (Decode.field "key" Decode.string)


toEditAction : String -> KeyboardAction
toEditAction string =
    let
        d =
            Debug.log "keyString " string

        action =
            case string of
                "q" ->
                    Change <| Quater

                "w" ->
                    Change <| Whole

                "h" ->
                    Change <| Half

                "e" ->
                    Change <| Eighth

                "n" ->
                    Add

                "d" ->
                    Delete

                "r" ->
                    SwitchRest

                "ArrowLeft" ->
                    Move Left

                "ArrowRight" ->
                    Move Right

                _ ->
                    None
    in
    Edit action



-- CONSTANTS


lineWidth : Float
lineWidth =
    4


timeSigWidth : Float
timeSigWidth =
    60


stemHeight : Float
stemHeight =
    44


stemWidth : Float
stemWidth =
    7


noteWidth : Float
noteWidth =
    42
