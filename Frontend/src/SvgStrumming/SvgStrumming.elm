port module SvgStrumming.SvgStrumming exposing (Model, initModel, tick, tickTime, updateBpm, view)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Html as Html
import Html.Attributes as HtmlAttributes
import Json.Decode exposing (..)
import Json.Encode as Encode
import Svg exposing (Attribute, Svg, circle, node, rect, svg)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
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


type alias ImgInfo =
    { lineWidth : Float
    , imgHeight : Float
    , imgWidth : Float
    , noteWidth : Float
    , xStart : Float
    }


type alias TimeSignature =
    ( Int, Int )


type alias Pattern =
    { notes : Cl.NonEmptyCyclicList Note
    , timeSignature : TimeSignature
    , bpm : Int
    , ticks : Int
    }


type alias Model =
    { info : ImgInfo, pattern : Pattern }


initModel : Model
initModel =
    { info = createDefaultImgInfo defaultPattern
    , pattern = defaultPattern
    }



-- LOGIC


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


createDefaultImgInfo : Pattern -> ImgInfo
createDefaultImgInfo pattern =
    let
        totalBeats =
            getTotalBeats pattern

        noteWidth =
            30

        xStart =
            noteWidth * 3

        imgWidth =
            noteWidth * 2 * toFloat totalBeats

        imgHeight =
            100
    in
    { lineWidth = 4
    , imgHeight = imgHeight
    , imgWidth = imgWidth
    , noteWidth = noteWidth
    , xStart = xStart
    }


defaultPattern2 : Pattern
defaultPattern2 =
    { notes = Cl.init (Note Whole) [ Note Whole, Note Whole, Note Half, Note Half ]
    , timeSignature = ( 4, 4 )
    , bpm = 70
    , ticks = 0
    }


defaultPattern : Pattern
defaultPattern =
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


updateBpm : Model -> Int -> Model
updateBpm ({ pattern } as model) bpm =
    let
        p =
            { pattern | bpm = bpm }
    in
    { model | pattern = p }


tick : Model -> ( Model, Bool, Cmd msg )
tick ({ pattern } as model) =
    let
        time =
            tickTime model

        ticks =
            pattern.ticks + 1

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
                { nextPattern | ticks = 0 }

            else
                { pattern | ticks = ticks }

        cmd =
            if moveNextNote then
                play (Encode.bool moveNextNote)

            else
                Cmd.none
    in
    ( { model | pattern = finalPattern }, moveNextNote, cmd )


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



-- VIEW


view : List (Element.Attribute msg) -> Model -> Element msg
view att model =
    let
        svgHtml : Svg msg
        svgHtml =
            renderPattern model

        metronomeClickHtml =
            Html.audio
                [ HtmlAttributes.id "metronome-play"
                , HtmlAttributes.src "click.mp3"
                , HtmlAttributes.controls False
                ]
                []
    in
    column att [ html svgHtml, html metronomeClickHtml ]


renderPattern : Model -> Svg msg
renderPattern ({ info, pattern } as model) =
    svg
        [ width (String.fromFloat info.imgWidth)
        , height (String.fromFloat info.imgHeight)
        , viewBox ("0 0 " ++ String.fromFloat info.imgWidth ++ " " ++ String.fromFloat info.imgHeight)
        ]
        (renderTimeSignature pattern.timeSignature
            ++ renderBarLines info pattern
            ++ renderMiddleLine info
            ++ renderNotes model
        )


renderTimeSignature : TimeSignature -> List (Svg msg)
renderTimeSignature ( top, bot ) =
    [ Svg.text_
        [ x "20"
        , y "45"
        , SA.fontSize "40"
        , SA.textAnchor "middle"
        , SA.fontWeight "bold"
        ]
        [ Svg.text (String.fromInt top) ]
    , Svg.text_
        [ x "20"
        , y "90"
        , SA.fontSize "40"
        , SA.textAnchor "middle"
        , SA.fontWeight "bold"
        ]
        [ Svg.text (String.fromInt bot) ]
    ]


renderBarLines : ImgInfo -> Pattern -> List (Svg msg)
renderBarLines info pattern =
    let
        line : Float -> Svg msg
        line xStart =
            Svg.rect
                [ width (String.fromFloat info.lineWidth)
                , height (String.fromFloat info.imgHeight)
                , x (String.fromFloat (xStart - info.noteWidth))
                ]
                []

        totalLines =
            getTotalBeats pattern // Tuple.first pattern.timeSignature

        linesStart =
            List.map (\x -> info.xStart + toFloat x * 4 * 2 * info.noteWidth) <|
                List.range 0 totalLines
    in
    List.map line linesStart


renderMiddleLine : ImgInfo -> List (Svg msg)
renderMiddleLine info =
    [ Svg.rect
        [ width (String.fromFloat info.imgWidth)
        , height (String.fromFloat info.lineWidth)
        , y (String.fromFloat (info.imgHeight / 2))
        , fill "gray"
        ]
        []
    ]


renderNotes : Model -> List (Svg msg)
renderNotes { info, pattern } =
    let
        -- get a list of all note to be shown, and map then with an X-position
        pos : Pos
        pos =
            { x = 0, y = info.imgHeight / 2 + (info.lineWidth / 2) }

        setPos =
            List.map (\( note, ( xS, end ) ) -> ( note, False, { pos | x = xS } ))

        setPosCurrent =
            List.map (\( note, ( xS, end ) ) -> ( note, True, { pos | x = xS } ))

        start =
            getStart info info.xStart

        before =
            mapNoteStartX info pattern.timeSignature pattern.notes.before (start [])

        current =
            mapNoteStartX info pattern.timeSignature [ pattern.notes.current ] (start before)

        after =
            mapNoteStartX info pattern.timeSignature pattern.notes.after (start current)

        notesWithStart =
            setPos before ++ setPosCurrent current ++ setPos after
    in
    flatMap (renderNote info) notesWithStart


getStart : ImgInfo -> Float -> List ( Note, ( Float, Float ) ) -> Float
getStart info default notes =
    case List.head <| List.reverse notes of
        Nothing ->
            default

        Just ( _, ( _, end ) ) ->
            end


renderNote : ImgInfo -> ( Note, Bool, Pos ) -> List (Svg msg)
renderNote info ( note, isCurrent, pos ) =
    let
        attribs =
            if isCurrent then
                [ SA.fill "blue" ]

            else
                []
    in
    case note of
        Note d ->
            case d of
                Whole ->
                    renderWhole attribs pos

                Half ->
                    renderHalf attribs info pos

                Quater ->
                    renderQuater attribs info pos

        Rest _ ->
            []


mapNoteStartX : ImgInfo -> TimeSignature -> List Note -> Float -> List ( Note, ( Float, Float ) )
mapNoteStartX info timeSig notes current =
    case notes of
        [] ->
            []

        n :: ns ->
            let
                dur =
                    info.noteWidth
                        * 2
                        * getNoteDuration timeSig n
            in
            ( n, ( current, current + dur ) ) :: mapNoteStartX info timeSig ns (current + dur)


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


renderWhole : List (Attribute msg) -> Pos -> List (Svg msg)
renderWhole attribs pos =
    [ Svg.ellipse
        ([ cx (String.fromFloat pos.x)
         , cy (String.fromFloat pos.y)
         , ry (String.fromFloat 18)
         , rx (String.fromFloat 21)
         ]
            ++ attribs
        )
        []
    , Svg.ellipse
        [ cx (String.fromFloat pos.x)
        , cy (String.fromFloat pos.y)
        , ry (String.fromFloat 12)
        , rx (String.fromFloat 8)
        , fill "white"
        ]
        []
    ]


renderQuater : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderQuater attribs info pos =
    let
        xP =
            pos.x

        yP =
            pos.y
    in
    [ renderStem attribs info { x = xP, y = yP }
    , rotate pos
        -45
        (Svg.ellipse
            ([ cx (String.fromFloat xP)
             , cy (String.fromFloat yP)
             , ry (String.fromFloat 10)
             , rx (String.fromFloat 12)
             ]
                ++ attribs
            )
            []
        )
    ]


renderStem : List (Attribute msg) -> ImgInfo -> Pos -> Svg msg
renderStem attribs info pos =
    let
        h =
            44
    in
    rect
        ([ width (String.fromFloat info.lineWidth)
         , height (String.fromFloat h)
         , x (String.fromFloat (pos.x + 7))
         , y (String.fromFloat (pos.y - h))
         ]
            ++ attribs
        )
        []


renderHalf : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderHalf attrib info pos =
    let
        xP =
            pos.x

        yP =
            pos.y

        h =
            45

        inner =
            rotate { x = xP, y = yP }
                -45
                (Svg.ellipse
                    [ cx (String.fromFloat xP)
                    , cy (String.fromFloat yP)
                    , ry (String.fromFloat 5)
                    , rx (String.fromFloat 9)
                    , fill "white"
                    ]
                    []
                )
    in
    renderQuater attrib info pos ++ [ inner ]


rotate : Pos -> Float -> Svg msg -> Svg msg
rotate pos deg el =
    let
        rotString =
            String.concat
                [ "rotate("
                , String.fromFloat deg
                , ","
                , String.fromFloat pos.x
                , ","
                , String.fromFloat pos.y
                , ")"
                ]
    in
    node "g" [ SA.transform rotString ] [ el ]
