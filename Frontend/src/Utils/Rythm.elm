port module Utils.Rythm exposing (Duration(..), Note(..), Rythm, TimeSignature, defaultRythm, getNoteDuration, getTotalBeats, tick, tickTime, updateBpm)

import Json.Encode as Encode
import Utils.NonEmptyCyclicList as Cl


port play : Encode.Value -> Cmd msg


type Note
    = Note Duration
    | Rest Duration


type Duration
    = Whole
    | Half
    | Quater


type alias Pos =
    { x : Float, y : Float }


type alias TimeSignature =
    ( Int, Int )


type alias Rythm =
    { notes : Cl.NonEmptyCyclicList Note
    , timeSignature : TimeSignature
    , bpm : Int
    , ticks : Int
    }



-- LOGIC


tickTime : Rythm -> Float
tickTime rythm =
    let
        beatLength =
            beatDuration rythm.bpm

        -- find lowest note duration and calc tick time based on that
        lowest =
            lowestDuration Quater (Cl.getAll rythm.notes)

        minBeatValue =
            getDurationLength rythm.timeSignature lowest
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
                    compareDuration init d
            in
            lowestDuration next ns


compareDuration : Duration -> Duration -> Duration
compareDuration a b =
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


getDuration : Note -> Duration
getDuration note =
    case note of
        Note d ->
            d

        Rest d ->
            d


getTotalBeats : Rythm -> Int
getTotalBeats { timeSignature, notes } =
    let
        partial =
            getNoteDurationInternal timeSignature

        total =
            round <| List.sum <| List.map partial <| Cl.getAll notes

        beatsPrBar =
            Tuple.first timeSignature

        rem =
            remainderBy beatsPrBar total
    in
    total + beatsPrBar - rem


defaultRythm2 : Rythm
defaultRythm2 =
    { notes = Cl.init (Note Whole) [ Note Whole, Note Whole, Note Half, Note Half ]
    , timeSignature = ( 4, 4 )
    , bpm = 70
    , ticks = 0
    }


defaultRythm : Rythm
defaultRythm =
    { notes = Cl.init (Note Whole) [ Note Whole, Note Whole, Note Half, Note Half ]
    , timeSignature = ( 4, 4 )
    , bpm = 70
    , ticks = 0
    }


flatMap : (a -> List b) -> List a -> List b
flatMap f l =
    case l of
        [] ->
            []

        n :: ns ->
            f n ++ flatMap f ns



-- UPDATE


updateBpm : Rythm -> Int -> Rythm
updateBpm rythm bpm =
    { rythm | bpm = bpm }


tick : Rythm -> ( Rythm, Bool, Cmd msg )
tick rythm =
    let
        time =
            tickTime rythm

        ticks =
            rythm.ticks + 1

        passed =
            toFloat ticks * time

        noteTime =
            noteTotalTime rythm

        moveNextNote =
            passed >= noteTime

        nextRythm =
            if moveNextNote then
                { rythm | notes = Cl.next rythm.notes }

            else
                rythm

        finalRythm =
            if moveNextNote then
                { nextRythm | ticks = 0 }

            else
                { rythm | ticks = ticks }

        cmd =
            if moveNextNote then
                play (Encode.bool moveNextNote)

            else
                Cmd.none
    in
    ( finalRythm, moveNextNote, cmd )


noteTotalTime : Rythm -> Float
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


getNoteDuration : Rythm -> Note -> Float
getNoteDuration { timeSignature } note =
    getNoteDurationInternal timeSignature note


getNoteDurationInternal : TimeSignature -> Note -> Float
getNoteDurationInternal timeSig note =
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
