module SvgStrumming.StrummingRender exposing (view)

import Element exposing (Element, column, html, text, wrappedRow)
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
    , beatsDuration : Float
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
            takeNotesForBar timeSignature ( [], beatsInBar, notes )
    in
    ( { beats = beatsInBar
      , notes = rNotes
      , beatOffset = initOverStep
      }
    , taken
    , remaining
    )


takeNotesForBar : TimeSignature -> ( List (RenderNote msg), Float, List ( Note, Bool ) ) -> ( List (RenderNote msg), Float, List ( Note, Bool ) )
takeNotesForBar ts ( rNotes, beatsLeft, notes ) =
    if beatsLeft > 0 then
        let
            ( note, taken, remaining ) =
                nextNote ts notes
        in
        case note of
            Nothing ->
                ( rNotes, -beatsLeft, notes )

            Just n ->
                takeNotesForBar ts ( rNotes ++ [ n ], beatsLeft - taken, remaining )

    else
        ( rNotes, -beatsLeft, notes )


nextNote : TimeSignature -> List ( Note, Bool ) -> ( Maybe (RenderNote msg), Float, List ( Note, Bool ) )
nextNote ts notes =
    case notes of
        ( n, isCurrent ) :: ns ->
            let
                f =
                    getRenderF n

                duration =
                    getNoteDuration ts n
            in
            ( Just { renderF = f, isCurrent = isCurrent, beatsDuration = duration }, duration, ns )

        [] ->
            ( Nothing, 0, [] )


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
            renderRest



-- VIEW


view : List (Element.Attribute msg) -> Model -> Element msg
view att model =
    let
        renderBars =
            toRenderBars model.pattern

        new =
            viewRenderBars model { x = 0, y = 0 } renderBars

        d =
            debug "model" (Cl.getAll model.pattern.notes)
    in
    column []
        [ new, text "Old" ]


viewRenderBars : Model -> Pos -> List (RenderBar msg) -> Element msg
viewRenderBars ({ info } as model) initPos bars =
    let
        --render timeSig
        ( svgList, endPos ) =
            renderBarsToSvg model initPos bars

        d =
            debug "endPos" endPos

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
                    renderNotes model (posAddX initPos (b.beatOffset * beatWidth + noteWidth)) b.notes

                fullBar =
                    barLines ++ notes
            in
            let
                ( remBars, end ) =
                    renderBarsToSvg model nextStart bs
            in
            ( fullBar ++ remBars, end )


renderNotes : Model -> Pos -> List (RenderNote msg) -> ( List (Svg msg), Pos )
renderNotes model pos notes =
    case notes of
        [] ->
            ( [], pos )

        n :: ns ->
            let
                ( noteSvg, nextStart ) =
                    renderNote model pos n
            in
            let
                ( remNotes, end ) =
                    renderNotes model nextStart ns
            in
            ( noteSvg ++ remNotes, end )


renderNote : Model -> Pos -> RenderNote msg -> ( List (Svg msg), Pos )
renderNote { info } pos { renderF, isCurrent, beatsDuration } =
    let
        attribs =
            if isCurrent then
                [ SA.fill "blue" ]

            else
                []

        svg =
            renderF attribs info <| posAddY pos (info.imgHeight / 2)
    in
    ( svg, posAddX pos (beatWidth * beatsDuration) )


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

        d =
            debug "remBars" ( pos, barWidth )
    in
    ( [ lineStart ] ++ lineEnd, posAdd pos { x = barWidth, y = 0 } )


beatWidth : Float
beatWidth =
    noteWidth * 2


renderRest : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderRest attribs info pos =
    [ rect
        ([ width (String.fromFloat 30)
         , height (String.fromFloat 20)
         , x (String.fromFloat pos.x)
         , y (String.fromFloat (pos.y - 10))
         ]
            ++ attribs
        )
        []
    ]


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


posAdd : Pos -> Pos -> Pos
posAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


posAddY : Pos -> Float -> Pos
posAddY p1 y =
    { p1 | y = p1.y + y }


posAddX : Pos -> Float -> Pos
posAddX p1 x =
    { p1 | x = p1.x + x }
