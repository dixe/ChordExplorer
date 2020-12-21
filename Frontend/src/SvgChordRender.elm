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
            renderChart model (renderFrettings model)
    in
    column []
        [ html (toHtml svgHtml)
        , button [] { label = text "Download", onPress = Just (DownloadSvg (toString 1 svgHtml)) }
        ]


renderFrettings : SvgModel -> List (Svg Msg)
renderFrettings model =
    List.concatMap (renderFretting model.info) (getFretsPos model.info model.frets)


renderFretting : ImgInfo -> ( Fret, Pos ) -> List (Svg Msg)
renderFretting info ( fret, pos ) =
    case fret of
        Open ->
            renderOpen info pos

        Mute ->
            renderMute info pos

        Fret fn ->
            renderPlay pos info.diameter


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


renderOpen : ImgInfo -> Pos -> List (Svg Msg)
renderOpen info pos =
    let
        inner =
            circle
                [ cx (String.fromFloat pos.x)
                , cy (String.fromFloat pos.y)
                , r (String.fromFloat (info.diameter * 2 - info.lineWidth / 2))
                , fill "white"
                ]
                []
    in
    renderPlay pos info.diameter ++ [ inner ]


renderMute : ImgInfo -> Pos -> List (Svg Msg)
renderMute info pos =
    renderX info pos.x pos.y


renderX : ImgInfo -> Float -> Float -> List (Svg Msg)
renderX info x y =
    let
        leftRight =
            renderXLeftRight info x y

        rightLeft =
            renderXRightLeft info x y
    in
    [ leftRight, rightLeft ]


renderXRightLeft : ImgInfo -> Float -> Float -> Svg Msg
renderXRightLeft info x y =
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


renderXLeftRight : ImgInfo -> Float -> Float -> Svg Msg
renderXLeftRight info x y =
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


renderChart : SvgModel -> List (Svg Msg) -> Svg.String.Html Msg
renderChart model nodes =
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 400 400"
        , onClickSvg
        ]
        (renderBaseChart model.info
            ++ nodes
        )


renderBaseChart : ImgInfo -> List (Svg Msg)
renderBaseChart info =
    let
        strings =
            List.map renderStringLine (List.map (\a -> { info | x = info.x + toFloat a * info.stringSpace }) (List.range 0 (info.numStrings - 1)))

        frets =
            List.map renderFretLine (List.map (\a -> { info | y = info.y + toFloat a * info.fretSpace }) (List.range 0 info.numFrets))

        nut =
            renderNut info
    in
    strings ++ frets ++ nut


renderNut : ImgInfo -> List (Svg Msg)
renderNut info =
    -- TODO if we add support for selecting starting pos then don'y show nut, but show fret Num as text
    [ rect
        [ x (String.fromFloat info.x)
        , y (String.fromFloat (info.y - info.lineWidth))
        , width (String.fromFloat (info.width + info.lineWidth))
        , height (String.fromFloat (info.lineWidth * 3))
        ]
        []
    ]


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
