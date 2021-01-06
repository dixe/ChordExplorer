module SvgStrumming.StrummingRender exposing (view)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Html as Html
import Html.Attributes as HtmlAttributes
import Svg exposing (Attribute, Svg, circle, node, rect, svg)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
import SvgStrumming.SvgStrumming exposing (..)
import Utils.NonEmptyCyclicList as Cl



-- maybe not correct since note can have longer duration that remaining bar length
-- can be solved with offset into next Renderbar


type alias RenderBar msg =
    { beats : Float
    , beatOffset : Float
    , notes : List (RenderNote msg)
    }


type alias RenderFunc msg =
    List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)


type alias RenderNote msg =
    { renderF : RenderFunc msg
    , isCurrent : Bool
    }


toRenderBars : Pattern -> List (RenderBar msg)
toRenderBars { notes, timeSignature } =
    let
        beats =
            4
    in
    { beats = beats
    , notes = Cl.mapWithCurrent noteToRenderNote pattern.notes
    , beatOffset = 0
    }


nextRenderBar : Pattern -> List Note -> ( RenderBar msg, Float, List Note )
nextRenderBar { timeSignature } notes =
    let
        beatsInBar =
            toFloat <|
                Tuple.first timeSignature

        -- take notes until we are over duration
        ( funs, taken, remaining ) =
            takeNotesForBar timeSignature ( [], beatsInBar, notes )

        -- calc how much we are over
        -- create renderBar from notes
        -- return renderBar, and overStep and remaining notes
    in
    ( { beats = beatsInBar
      , notes = List.map (\x -> { renderF = x, isCurrent = False }) funs --TODO also keep track of current
      , beatOffset = 0
      }
    , taken
    , remaining
    )


takeNotesForBar : TimeSignature -> ( List (RenderFunc msg), Float, List Note ) -> ( List (RenderFunc msg), Float, List Note )
takeNotesForBar ts ( funcs, beatsLeft, notes ) =
    if beatsLeft > 0 then
        let
            ( renderF, taken, remaining ) =
                nextNote ts notes
        in
        case renderF of
            Nothing ->
                ( funcs, -beatsLeft, notes )

            Just f ->
                takeNotesForBar ts ( funcs ++ [ f ], beatsLeft - taken, remaining )

    else
        ( funcs, -beatsLeft, notes )


nextNote : TimeSignature -> List Note -> ( Maybe (RenderFunc msg), Float, List Note )
nextNote ts notes =
    case notes of
        n :: ns ->
            let
                f =
                    getRenderF n

                duration =
                    getNoteDuration ts n
            in
            ( Just f, duration, ns )

        [] ->
            ( Nothing, 0, [] )


noteToRenderNote : Note -> RenderNote msg
noteToRenderNote note =
    { renderF = getRenderF note
    , isCurrent = False
    }


getRenderF : Note -> RenderFunc msg
getRenderF note =
    case note of
        Note d ->
            case d of
                Whole ->
                    renderWhole

                Half ->
                    renderHalf

                Quater ->
                    renderQuater

                Eighth ->
                    renderEigth

        Rest _ ->
            \_ _ _ -> []



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
                    renderWhole attribs info newPos

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


renderWhole : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderWhole attribs _ pos =
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


flatMap : (a -> List b) -> List a -> List b
flatMap f l =
    case l of
        [] ->
            []

        n :: ns ->
            f n ++ flatMap f ns


debug : String -> a -> a
debug label a =
    Debug.log label a
