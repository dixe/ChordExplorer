module SvgRythm.SvgRythm exposing (Model, initModel, view)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Html as Html
import Html.Attributes as HtmlAttributes
import Json.Decode exposing (..)
import Json.Encode as Encode
import Svg exposing (Attribute, Svg, circle, node, rect, svg)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
import Utils.Rythm exposing (Duration(..), Note(..), Rythm, TimeSignature, getNoteDuration, getTotalBeats)


type alias Pos =
    { x : Float, y : Float }


type alias ImgInfo =
    { lineWidth : Float
    , imgHeight : Float
    , imgWidth : Float
    , noteWidth : Float
    , xStart : Float
    }


type alias Model =
    { info : ImgInfo }


initModel : Rythm -> Model
initModel rythm =
    { info = createDefaultImgInfo rythm
    }


createDefaultImgInfo : Rythm -> ImgInfo
createDefaultImgInfo rythm =
    let
        totalBeats =
            getTotalBeats rythm

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


flatMap : (a -> List b) -> List a -> List b
flatMap f l =
    case l of
        [] ->
            []

        n :: ns ->
            f n ++ flatMap f ns



-- VIEW


view : List (Element.Attribute msg) -> Rythm -> Model -> Element msg
view att rythm model =
    let
        svgHtml : Svg msg
        svgHtml =
            renderRythm rythm model

        metronomeClickHtml =
            Html.audio
                [ HtmlAttributes.id "metronome-play"
                , HtmlAttributes.src "click.mp3"
                , HtmlAttributes.controls False
                ]
                []
    in
    column att [ html svgHtml, html metronomeClickHtml ]


renderRythm : Rythm -> Model -> Svg msg
renderRythm rythm ({ info } as model) =
    svg
        [ width (String.fromFloat info.imgWidth)
        , height (String.fromFloat info.imgHeight)
        , viewBox ("0 0 " ++ String.fromFloat info.imgWidth ++ " " ++ String.fromFloat info.imgHeight)
        ]
        (renderTimeSignature rythm.timeSignature
            ++ renderBarLines info rythm
            ++ renderMiddleLine info
            ++ renderNotes rythm model
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


renderBarLines : ImgInfo -> Rythm -> List (Svg msg)
renderBarLines info rythm =
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
            getTotalBeats rythm // Tuple.first rythm.timeSignature

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


renderNotes : Rythm -> Model -> List (Svg msg)
renderNotes rythm { info } =
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
            mapNoteStartX info rythm rythm.notes.before (start [])

        current =
            mapNoteStartX info rythm [ rythm.notes.current ] (start before)

        after =
            mapNoteStartX info rythm rythm.notes.after (start current)

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


mapNoteStartX : ImgInfo -> Rythm -> List Note -> Float -> List ( Note, ( Float, Float ) )
mapNoteStartX info rythm notes current =
    case notes of
        [] ->
            []

        n :: ns ->
            let
                dur =
                    info.noteWidth
                        * 2
                        * getNoteDuration rythm n
            in
            ( n, ( current, current + dur ) ) :: mapNoteStartX info rythm ns (current + dur)


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
