module SvgChordRender exposing (createChordView)

import Decoders exposing (..)
import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Svg.String exposing (Attribute, Svg, circle, node, rect, svg, toHtml, toString)
import Svg.String.Attributes exposing (..)
import Svg.String.Events exposing (..)
import SvgChordLogic exposing (..)
import Types exposing (..)


createChordView : SvgModel -> Element Msg
createChordView model =
    let
        svgHtml =
            renderBaseChart model (getRenderNodes model)
    in
    column []
        [ html (toHtml svgHtml)
        , button [] { label = text "Download", onPress = Just (DownloadSvg (toString 0 svgHtml)) }
        ]


getRenderNodes : SvgModel -> List (Svg Msg)
getRenderNodes model =
    List.concatMap (renderFret model.info) model.frets


renderFret : ImgInfo -> Fret -> List (Svg Msg)
renderFret info fret =
    let
        pos =
            getFretPos info fret
    in
    case getFretType fret of
        Play ->
            renderPlay pos info.diameter

        Mute ->
            renderMute info pos


renderPlay : Pos -> Float -> List (Svg Msg)
renderPlay pos dia =
    let
        radius =
            dia * 2
    in
    [ circle
        [ cx (String.fromFloat pos.x)
        , cy (String.fromFloat pos.y)
        , r (String.fromFloat radius)
        ]
        []
    ]


renderMute : ImgInfo -> Pos -> List (Svg Msg)
renderMute info pos =
    renderX info pos.x pos.y


renderX : ImgInfo -> Float -> Float -> List (Svg Msg)
renderX info x y =
    let
        leftRight =
            renderLeftRight info x y

        rightLeft =
            renderRightLeft info x y
    in
    [ leftRight, rightLeft ]


renderRightLeft : ImgInfo -> Float -> Float -> Svg Msg
renderRightLeft info x y =
    let
        radius =
            info.diameter * 2

        x1V =
            x + radius * cos (degrees -45)

        x2V =
            x + radius * cos (degrees -225)

        y1V =
            y + radius * sin (degrees -45)

        y2V =
            y + radius * sin (degrees -225)
    in
    line
        [ x1 x1V
        , y1 y1V
        , x2 x2V
        , y2 y2V
        , strokeWidth (String.fromFloat info.lineWidth)
        ]
        []


renderLeftRight : ImgInfo -> Float -> Float -> Svg Msg
renderLeftRight info x y =
    let
        radius =
            info.diameter * 2

        x1V =
            x + radius * cos (degrees 45)

        x2V =
            x + radius * cos (degrees 225)

        y1V =
            y + radius * sin (degrees 45)

        y2V =
            y + radius * sin (degrees 225)
    in
    line
        [ x1 x1V
        , y1 y1V
        , x2 x2V
        , y2 y2V
        , strokeWidth (String.fromFloat info.lineWidth)
        ]
        []


onClickSvg : Attribute Msg
onClickSvg =
    on "click" mouseXY


renderBaseChart : SvgModel -> List (Svg Msg) -> Svg.String.Html Msg
renderBaseChart model nodes =
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 400 400"
        , onClickSvg
        ]
        (renderStrings model.info
            ++ nodes
        )


renderStrings : ImgInfo -> List (Svg Msg)
renderStrings info =
    let
        strings =
            List.map renderStringLine (List.map (\a -> { info | x = info.x + toFloat a * info.stringSpace }) (List.range 0 (info.numStrings - 1)))

        frets =
            List.map renderFretLine (List.map (\a -> { info | y = info.y + toFloat a * info.fretSpace }) (List.range 0 info.numFrets))
    in
    strings ++ frets


renderStringLine : ImgInfo -> Svg Msg
renderStringLine info =
    rect
        [ x (String.fromFloat info.x)
        , y (String.fromFloat info.y)
        , width (String.fromFloat info.lineWidth)
        , height (String.fromFloat (info.height + info.lineWidth))
        ]
        []


renderFretLine : ImgInfo -> Svg Msg
renderFretLine info =
    rect
        [ x (String.fromFloat info.x)
        , y (String.fromFloat info.y)
        , width (String.fromFloat (info.width + info.lineWidth))
        , height (String.fromFloat info.lineWidth)
        ]
        []



-- Svg.String helper and 'extensions'


x1 : Float -> Attribute Msg
x1 value =
    attribute "x1" (String.fromFloat value)


x2 : Float -> Attribute Msg
x2 value =
    attribute "x2" (String.fromFloat value)


y1 : Float -> Attribute Msg
y1 value =
    attribute "y1" (String.fromFloat value)


y2 : Float -> Attribute Msg
y2 value =
    attribute "y2" (String.fromFloat value)


line : List (Attribute Msg) -> List (Svg Msg) -> Svg Msg
line attribs children =
    node "line" (attribs ++ [ attribute "stroke" "black" ]) children
