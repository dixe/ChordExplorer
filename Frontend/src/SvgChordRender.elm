module SvgChordRender exposing (createChordView)

import Decoders exposing (..)
import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Svg.String exposing (Attribute, Svg, circle, rect, svg, toHtml, toString)
import Svg.String.Attributes exposing (..)
import Svg.String.Events exposing (..)
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
    case model.clickPos of
        Just pos ->
            [ renderClick pos ]

        Nothing ->
            []


onClickSvg : Attribute Msg
onClickSvg =
    on "click" mouseXY


renderClick : Pos -> Svg Msg
renderClick pos =
    circle
        [ cx (String.fromFloat pos.x)
        , cy (String.fromFloat pos.y)
        , r (String.fromFloat 10)
        ]
        []


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
            List.map renderString (List.map (\a -> { info | x = info.x + a * info.stringSpace }) (List.range 0 (info.numStrings - 1)))

        frets =
            List.map renderFret (List.map (\a -> { info | y = info.y + a * info.fretSpace }) (List.range 0 info.numFrets))
    in
    strings ++ frets


renderString : ImgInfo -> Svg Msg
renderString info =
    rect
        [ x (String.fromInt info.x)
        , y (String.fromInt info.y)
        , width (String.fromInt info.lineWidth)
        , height (String.fromInt (info.height + info.lineWidth))
        ]
        []


renderFret : ImgInfo -> Svg Msg
renderFret info =
    rect
        [ x (String.fromInt info.x)
        , y (String.fromInt info.y)
        , width (String.fromInt (info.width + info.lineWidth))
        , height (String.fromInt info.lineWidth)
        ]
        []
