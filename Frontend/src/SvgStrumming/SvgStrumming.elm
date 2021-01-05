port module SvgStrumming.SvgStrumming exposing (EditAction, Model, Note, initModel, noteDecoder, tick, tickTime, updateAndAdvance, updateBpm, view)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Html as Html
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode
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


type EditAction
    = Change Note
    | Add
    | Move Direction
    | None
    | Delete


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
    { info = createImgInfo defaultPattern
    , pattern = defaultPattern
    }



-- LOGIC


updateAndAdvance : Model -> EditAction -> Model
updateAndAdvance ({ pattern } as model) note =
    case note of
        Change n ->
            { model | pattern = { pattern | notes = Cl.next <| Cl.updateCurrent n pattern.notes } }

        Add ->
            let
                newP =
                    { pattern | notes = Cl.add (Note Quater) pattern.notes }

                newInfo =
                    createImgInfo newP

                d =
                    Debug.log "NewInfo" newInfo
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

        None ->
            model


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
            b

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

        d =
            Debug.log "totalBeats" (total + beatsPrBar - rem)
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

        d =
            Debug.log "barWidth, ImgWidth" ( barsWidth, imgWidth )
    in
    { imgHeight = imgHeight
    , imgWidth = imgWidth
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
    { notes = Cl.init (Note Eighth) [ Note Whole, Note Whole, Note Half, Note Half ]
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
{-

   update : Msg -> Model -> ( Model, Cmd Msg )
   update msg model =
       case msg of
           Edit ->
               ( { model | state = Editing }, Cmd.none )

           FinishEdit ->
               ( { model | state = Playing }, Cmd.none )
-}


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



-- Subscription decoders


noteDecoder : Decode.Decoder EditAction
noteDecoder =
    Decode.map toEditAction (Decode.field "key" Decode.string)


toEditAction : String -> EditAction
toEditAction string =
    let
        d =
            Debug.log "keyString " string
    in
    case string of
        "q" ->
            Change <| Note Quater

        "w" ->
            Change <| Note Whole

        "h" ->
            Change <| Note Half

        "e" ->
            Change <| Note Eighth

        "n" ->
            Add

        "d" ->
            Delete

        "ArrowLeft" ->
            Move Left

        "ArrowRight" ->
            Move Right

        _ ->
            None



-- VIEW


getTotalBars : Pattern -> Int
getTotalBars ({ timeSignature, notes } as pattern) =
    getTotalBeats pattern // Tuple.first pattern.timeSignature


getBarWidth : Pattern -> Float
getBarWidth ({ timeSignature, notes } as pattern) =
    let
        beats =
            toFloat <| Tuple.first timeSignature

        d =
            Debug.log "Beats, notewidth" ( beats, noteWidth )
    in
    timeSigWidth + beats * 2 * noteWidth + lineWidth


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

        elements =
            [ html svgHtml, html metronomeClickHtml ]
    in
    column att elements


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
            ++ renderRythmBox info
        )


renderRythmBox : ImgInfo -> List (Svg msg)
renderRythmBox info =
    [ Svg.rect
        [ width <| String.fromFloat info.imgWidth
        , height "1"
        ]
        []
    , Svg.rect
        [ width <| String.fromFloat info.imgWidth
        , height "1"
        , y <| String.fromFloat (info.imgHeight - 1)
        ]
        []
    , Svg.rect
        [ height <| String.fromFloat info.imgHeight
        , width "1"
        ]
        []
    , Svg.rect
        [ height <| String.fromFloat info.imgHeight
        , width "1"
        , x <| String.fromFloat (info.imgWidth - 1)
        ]
        []
    ]


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
                [ width (String.fromFloat lineWidth)
                , height (String.fromFloat info.imgHeight)
                , x (String.fromFloat xStart)
                ]
                []

        bars =
            getTotalBars pattern

        barWidth =
            getBarWidth pattern

        linesStart =
            List.map (\x -> timeSigWidth + x * barWidth) <|
                List.map toFloat <|
                    List.range 0 bars

        d =
            Debug.log "xStart , lineStart" ( timeSigWidth, linesStart )
    in
    List.map line linesStart


renderMiddleLine : ImgInfo -> List (Svg msg)
renderMiddleLine info =
    [ Svg.rect
        [ width (String.fromFloat info.imgWidth)
        , height (String.fromFloat lineWidth)
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
            { x = 0, y = info.imgHeight / 2 + (lineWidth / 2) }

        setPos =
            List.map (\( note, ( xS, end ) ) -> ( note, False, { pos | x = xS } ))

        setPosCurrent =
            List.map (\( note, ( xS, end ) ) -> ( note, True, { pos | x = xS } ))

        start =
            getStart timeSigWidth

        before =
            mapNoteStartX info pattern pattern.notes.before (start [])

        current =
            mapNoteStartX info pattern [ pattern.notes.current ] (start before)

        after =
            mapNoteStartX info pattern pattern.notes.after (start current)

        notesWithStart =
            setPos before ++ setPosCurrent current ++ setPos after
    in
    flatMap (renderNote info) notesWithStart


getStart : Float -> List ( Note, ( Float, Float ) ) -> Float
getStart default notes =
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

        newPos =
            { pos | x = pos.x + noteWidth / 2 }
    in
    case note of
        Note d ->
            case d of
                Whole ->
                    renderWhole attribs newPos

                Half ->
                    renderHalf attribs info newPos

                Quater ->
                    renderQuater attribs info newPos

                Eighth ->
                    renderEigth attribs info newPos

        Rest _ ->
            []


mapNoteStartX : ImgInfo -> Pattern -> List Note -> Float -> List ( Note, ( Float, Float ) )
mapNoteStartX info ({ timeSignature } as pattern) notes current =
    case notes of
        [] ->
            []

        n :: ns ->
            let
                barWidth =
                    getBarWidth pattern

                beats =
                    toFloat <| Tuple.first pattern.timeSignature

                beatWidth =
                    barWidth / beats

                xOffset =
                    beatWidth
                        * getNoteDuration pattern.timeSignature n

                d =
                    debug "xOffset " xOffset
            in
            ( n, ( current, current + xOffset ) ) :: mapNoteStartX info pattern ns (current + xOffset)


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
    35


renderStem : List (Attribute msg) -> ImgInfo -> Pos -> Svg msg
renderStem attribs info pos =
    rect
        ([ width (String.fromFloat lineWidth)
         , height (String.fromFloat stemHeight)
         , x (String.fromFloat (pos.x + stemWidth))
         , y (String.fromFloat (pos.y - stemHeight))
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


renderEigth : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderEigth attrib info pos =
    let
        xP =
            pos.x

        yP =
            pos.y

        flag =
            Svg.path
                (attrib
                    ++ [ SA.d <| flagPathString pos
                       ]
                )
                []
    in
    renderQuater attrib info pos ++ [ flag ]


flagPathString : Pos -> String
flagPathString pos =
    let
        xP =
            pos.x + lineWidth + stemWidth

        yP =
            pos.y - stemHeight

        d =
            Debug.log "Path pos" ( xP, yP )
    in
    "M "
        ++ String.fromFloat xP
        ++ " "
        ++ String.fromFloat yP
        ++ " c 0 10  10 12 10 26 "
        ++ " c 0 5  0 5  -5 10 "
        ++ " c 3 -5  5 -15 -5  -20 "


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



-- Helper


debug : String -> a -> a
debug label a =
    Debug.log label a
