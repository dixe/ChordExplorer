module SvgStrumming.SvgStrumming exposing (Model, initModel, view)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Json.Decode exposing (..)
import Svg exposing (Attribute, Svg, circle, node, rect, svg)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
import SvgChord.Logic exposing (..)
import SvgChord.Types exposing (..)


type Note
    = Note Duration
    | Rest Duration


type Duration
    = Whole
    | Half
    | Quater


type alias ImgInfo =
    { lineWidth : Float
    , imgHeight : Float
    , imgWidth : Float
    }


type alias Model =
    { info : ImgInfo }


initModel : Model
initModel =
    { info = createDefaultImgInfo }


createDefaultImgInfo : ImgInfo
createDefaultImgInfo =
    let
        imgWidth =
            600

        imgHeight =
            100
    in
    { lineWidth = 4
    , imgHeight = imgHeight
    , imgWidth = imgWidth
    }


view : List (Element.Attribute msg) -> Model -> Element msg
view att model =
    let
        svgHtml : Svg msg
        svgHtml =
            renderPattern model
    in
    column att [ html svgHtml ]


renderPattern : Model -> Svg msg
renderPattern { info } =
    let
        pos =
            { x = 50, y = info.imgHeight / 2 + (info.lineWidth / 2) }
    in
    svg
        [ width (String.fromFloat info.imgWidth)
        , height (String.fromFloat info.imgHeight)
        , viewBox ("0 0 " ++ String.fromFloat info.imgWidth ++ " " ++ String.fromFloat info.imgHeight)
        ]
        (renderWhole pos
            ++ renderQuater { pos | x = 150 }
            ++ [ Svg.rect
                    [ width (String.fromFloat info.imgWidth)
                    , height (String.fromFloat info.lineWidth)
                    , y (String.fromFloat (info.imgHeight / 2))
                    ]
                    []
               ]
        )


renderWhole : Pos -> List (Svg msg)
renderWhole pos =
    [ Svg.ellipse
        [ cx (String.fromFloat pos.x)
        , cy (String.fromFloat pos.y)
        , ry (String.fromFloat 18)
        , rx (String.fromFloat 21)
        ]
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


renderQuater : Pos -> List (Svg msg)
renderQuater pos =
    let
        xP =
            pos.x

        yP =
            pos.y
    in
    [ rotate pos
        -45
        (Svg.ellipse
            [ cx (String.fromFloat xP)
            , cy (String.fromFloat yP)
            , ry (String.fromFloat 10)
            , rx (String.fromFloat 12)
            ]
            []
        )
    ]


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
