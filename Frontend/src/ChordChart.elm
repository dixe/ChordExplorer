module ChordChart exposing (createChordView)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Svg.String exposing (Svg, rect, svg, toHtml, toString)
import Svg.String.Attributes exposing (..)
import Svg.String.Events exposing (..)
import Types exposing (..)


createChordView : Element Msg
createChordView =
    let
        svgHtml =
            renderBaseChart
    in
    column []
        [ html (toHtml svgHtml)
        , button [] { label = text "Download", onPress = Just (DownloadSvg (toString 0 svgHtml)) }
        ]


renderBaseChart : Svg.String.Html Msg
renderBaseChart =
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 400 400"
        , onClick (SvgClick 0 0)
        ]
        (renderStrings createImgInfo)


createImgInfo : ImgInfo
createImgInfo =
    let
        stringSpace =
            40

        fretSpace =
            70

        numStrings =
            6

        numFrets =
            4

        imgHeight =
            fretSpace * numFrets

        imgWidth =
            stringSpace * (numStrings - 1)
    in
    { x = 30
    , y = 30
    , lineWidth = 4
    , width = imgWidth
    , height = imgHeight
    , stringSpace = stringSpace
    , fretSpace = fretSpace
    , numStrings = numStrings
    , numFrets = numFrets
    }


renderStrings : ImgInfo -> List (Svg Msg)
renderStrings info =
    let
        strings =
            List.map renderString (List.map (\a -> { info | x = info.x + a * info.stringSpace }) (List.range 0 (info.numStrings - 1)))

        frets =
            List.map renderFret (List.map (\a -> { info | y = info.y + a * info.fretSpace }) (List.range 0 info.numFrets))
    in
    strings ++ frets


type alias ImgInfo =
    { x : Int, y : Int, lineWidth : Int, width : Int, height : Int, stringSpace : Int, fretSpace : Int, numStrings : Int, numFrets : Int }


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
