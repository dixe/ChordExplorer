module SvgStrumming.StrummingRender exposing (view)

import Element exposing (Element, column, html, text, wrappedRow)
import Element.Input exposing (button)
import Html as Html
import Html.Attributes as HtmlAttributes
import Svg exposing (Attribute, Svg, circle, node, rect, svg)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
import SvgStrumming.RenderNotes as RN exposing (beatWidth)
import SvgStrumming.SvgStrumming exposing (..)
import Utils.NonEmptyCyclicList as Cl



-- maybe not correct since note can have longer duration that remaining bar length
-- can be solved with offset into next Renderbar


type alias RenderBar msg =
    { beats : Float
    , beatOffset : Float
    , notes : List (RN.RenderNote msg)
    }


toRenderBars : Pattern -> List (RenderBar msg)
toRenderBars { notes, timeSignature } =
    toBars timeSignature 0 <| Cl.toListWithCurrent notes


toBars : TimeSignature -> Float -> List ( Note, Bool ) -> List (RenderBar msg)
toBars timeSignature initOverStep notes =
    let
        ( next, overStep, remaining ) =
            nextRenderBar timeSignature initOverStep notes

        rest =
            if remaining == [] then
                []

            else
                toBars timeSignature overStep remaining
    in
    next :: rest


nextRenderBar : TimeSignature -> Float -> List ( Note, Bool ) -> ( RenderBar msg, Float, List ( Note, Bool ) )
nextRenderBar timeSignature initOverStep notes =
    let
        beatsInBar =
            toFloat <|
                Tuple.first timeSignature

        ( rNotes, taken, remaining ) =
            takeNotesForBar timeSignature ( [], beatsInBar - initOverStep, notes )
    in
    ( { beats = beatsInBar
      , notes = rNotes
      , beatOffset = initOverStep
      }
    , taken
    , remaining
    )


takeNotesForBar : TimeSignature -> ( List (RN.RenderNote msg), Float, List ( Note, Bool ) ) -> ( List (RN.RenderNote msg), Float, List ( Note, Bool ) )
takeNotesForBar ts ( rNotes, beatsLeft, notes ) =
    if beatsLeft > 0 then
        let
            ( note, taken, remaining ) =
                nextNote ts notes
        in
        case note of
            Nothing ->
                ( rNotes, -beatsLeft, remaining )

            Just n ->
                takeNotesForBar ts ( rNotes ++ [ n ], beatsLeft - taken, remaining )

    else
        ( rNotes, -beatsLeft, notes )


nextNote : TimeSignature -> List ( Note, Bool ) -> ( Maybe (RN.RenderNote msg), Float, List ( Note, Bool ) )
nextNote ts notes =
    case notes of
        ( n, isCurrent ) :: ns ->
            let
                f =
                    RN.getRenderF n

                duration =
                    getNoteDuration ts n
            in
            ( Just { renderF = f, isCurrent = isCurrent, beatsDuration = duration }, duration, ns )

        [] ->
            ( Nothing, 0, [] )



-- VIEW


view : List (Element.Attribute msg) -> Model -> Element msg
view att model =
    let
        metronomeClickHtml =
            html <|
                Html.audio
                    [ HtmlAttributes.id "metronome-play"
                    , HtmlAttributes.src "click.mp3"
                    , HtmlAttributes.controls False
                    ]
                    []

        renderBars =
            toRenderBars model.pattern

        bars =
            viewRenderBars model { x = 0, y = 0 } renderBars
    in
    column []
        [ metronomeClickHtml, bars ]


viewRenderBars : Model -> Pos -> List (RenderBar msg) -> Element msg
viewRenderBars ({ info } as model) initPos bars =
    let
        --render timeSig
        ( svgList, endPos ) =
            renderBarsToSvg model initPos bars

        renderBox =
            renderRythmBox <| posAddY endPos info.imgHeight

        middleLine =
            renderMiddleLine <| posAddY endPos info.imgHeight

        svgWidth =
            endPos.x
    in
    html <|
        svg
            [ width (String.fromFloat svgWidth)
            , height (String.fromFloat info.imgHeight)
            , viewBox ("0 0 " ++ String.fromFloat svgWidth ++ " " ++ String.fromFloat info.imgHeight)
            ]
            (middleLine
                ++ svgList
                ++ renderBox
            )


renderMiddleLine : Pos -> List (Svg msg)
renderMiddleLine pos =
    [ Svg.rect
        [ width (String.fromFloat pos.x)
        , height (String.fromFloat lineWidth)
        , y (String.fromFloat (pos.y / 2))
        , fill "gray"
        ]
        []
    ]


renderRythmBox : Pos -> List (Svg msg)
renderRythmBox pos =
    [ Svg.rect
        [ width <| String.fromFloat pos.x
        , height "1"
        ]
        []
    , Svg.rect
        [ width <| String.fromFloat pos.x
        , height "1"
        , y <| String.fromFloat (pos.y - 1)
        ]
        []
    , Svg.rect
        [ height <| String.fromFloat pos.y
        , width "1"
        ]
        []
    , Svg.rect
        [ height <| String.fromFloat pos.y
        , width "1"
        , x <| String.fromFloat (pos.x - 1)
        ]
        []
    ]


renderBarsToSvg : Model -> Pos -> List (RenderBar msg) -> ( List (Svg msg), Pos )
renderBarsToSvg model initPos bars =
    case bars of
        [] ->
            ( [], initPos )

        b :: bs ->
            let
                ( barLines, nextStart ) =
                    renderSingleBarLines model.info initPos b.beats (bs == [])

                ( notes, _ ) =
                    RN.renderNotes model (posAddX initPos (b.beatOffset * beatWidth + noteWidth)) b.notes

                fullBar =
                    barLines ++ notes
            in
            let
                ( remBars, end ) =
                    renderBarsToSvg model nextStart bs
            in
            ( fullBar ++ remBars, end )


renderSingleBarLines : ImgInfo -> Pos -> Float -> Bool -> ( List (Svg msg), Pos )
renderSingleBarLines info pos beats isLast =
    let
        line : Float -> Svg msg
        line xStart =
            Svg.rect
                [ width (String.fromFloat lineWidth)
                , height (String.fromFloat info.imgHeight)
                , x (String.fromFloat xStart)
                ]
                []

        barWidth =
            noteWidth + beatWidth * beats + lineWidth

        lineStart =
            line pos.x

        lineEnd =
            if isLast then
                [ line <| pos.x + barWidth - lineWidth
                ]

            else
                []
    in
    ( [ lineStart ] ++ lineEnd, posAdd pos { x = barWidth, y = 0 } )



-- Helper


debug : String -> a -> a
debug label a =
    Debug.log label a


posAdd : Pos -> Pos -> Pos
posAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


posAddY : Pos -> Float -> Pos
posAddY p1 y =
    { p1 | y = p1.y + y }


posAddX : Pos -> Float -> Pos
posAddX p1 x =
    { p1 | x = p1.x + x }
