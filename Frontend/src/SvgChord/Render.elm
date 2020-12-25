module SvgChord.Render exposing (createChordView, getSvgString)

import Element exposing (Element, column, html, text)
import Element.Input exposing (button)
import Json.Decode exposing (..)
import Svg.String exposing (Attribute, Svg, circle, node, rect, svg, toHtml, toString)
import Svg.String.Attributes exposing (..)
import Svg.String.Events exposing (..)
import SvgChord.Logic exposing (..)
import SvgChord.Types exposing (..)


createChordView : SvgModel -> (Float -> Float -> msg) -> Element msg
createChordView model msgFun =
    let
        svgHtml : Svg.String.Html msg
        svgHtml =
            renderChart model msgFun
    in
    column []
        [ html (toHtml svgHtml) ]


renderName : SvgModel -> ( List (Svg msg), Pos )
renderName model =
    case model.name of
        "" ->
            ( [], { x = 0, y = 0 } )

        name ->
            let
                fontSizeInt =
                    28

                offset =
                    { x = 0
                    , y = nameHeight
                    }

                nameSvg =
                    Svg.String.text_
                        [ x (String.fromFloat ((model.info.width / 2) + model.info.x))
                        , y (String.fromInt fontSizeInt)
                        , fontSize (String.fromInt fontSizeInt)
                        , textAnchor "middle"
                        , attribute "font-weight" "bold"
                        ]
                        [ Svg.String.text model.name ]
            in
            ( [ nameSvg ], offset )


type Dummy
    = None


getSvgString : SvgModel -> Bool -> String
getSvgString model indent =
    let
        dummyFun : Float -> Float -> Dummy
        dummyFun =
            \f f1 -> None

        svgHtml : Svg.String.Html Dummy
        svgHtml =
            renderChart model dummyFun
    in
    toString
        (if indent then
            1

         else
            0
        )
        svgHtml


renderFrettings : SvgModel -> List (Svg msg)
renderFrettings model =
    List.concatMap (renderFretting model.info) (getFretsPos model.info model.frets)


renderFretting : ImgInfo -> ( Fret, Pos ) -> List (Svg msg)
renderFretting info ( fret, pos ) =
    case fret of
        Open ->
            renderOpen info pos

        Mute ->
            renderMute info pos

        Fret fn ->
            renderPlay pos info.radius


renderPlay : Pos -> Float -> List (Svg msg)
renderPlay pos radius =
    [ circle
        [ cx (String.fromFloat pos.x)
        , cy (String.fromFloat pos.y)
        , r (String.fromFloat radius)
        ]
        []
    ]


renderOpen : ImgInfo -> Pos -> List (Svg msg)
renderOpen info pos =
    let
        inner =
            circle
                [ cx (String.fromFloat pos.x)
                , cy (String.fromFloat pos.y)
                , r (String.fromFloat (info.radius - info.lineWidth / 2))
                , fill "white"
                ]
                []
    in
    renderPlay pos info.radius ++ [ inner ]


renderMute : ImgInfo -> Pos -> List (Svg msg)
renderMute info pos =
    renderX info pos.x pos.y


renderX : ImgInfo -> Float -> Float -> List (Svg msg)
renderX info x y =
    let
        leftRight =
            renderXLeftRight info x y

        rightLeft =
            renderXRightLeft info x y
    in
    [ leftRight, rightLeft ]


renderXRightLeft : ImgInfo -> Float -> Float -> Svg msg
renderXRightLeft info x y =
    let
        radius =
            info.radius

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


renderXLeftRight : ImgInfo -> Float -> Float -> Svg msg
renderXLeftRight info x y =
    let
        radius =
            info.radius

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


onClickSvg : (Float -> Float -> msg) -> Attribute msg
onClickSvg msgFun =
    on "click" (mouseXY msgFun)


renderChart : SvgModel -> (Float -> Float -> msg) -> Svg.String.Html msg
renderChart model msgFun =
    let
        ( svgName, offSetPos ) =
            renderName model

        adjustedModel =
            posAdjustedModel model offSetPos

        frettings =
            renderFrettings adjustedModel
    in
    svg
        [ width (String.fromFloat model.info.imgWidth)
        , height (String.fromFloat model.info.imgHeight)
        , viewBox ("0 0 " ++ String.fromFloat model.info.imgWidth ++ " " ++ String.fromFloat model.info.imgHeight)
        , onClickSvg msgFun
        ]
        (svgName
            ++ renderBaseChart adjustedModel.info
            ++ frettings
        )


posAdjustedModel : SvgModel -> Pos -> SvgModel
posAdjustedModel model pos =
    { model | info = posAdjustedInfo model.info pos }


posAdjustedInfo : ImgInfo -> Pos -> ImgInfo
posAdjustedInfo info offSet =
    { info | x = info.x + offSet.x, y = info.y + offSet.y }


renderBaseChart : ImgInfo -> List (Svg msg)
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


renderNut : ImgInfo -> List (Svg msg)
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


renderStringLine : ImgInfo -> Svg msg
renderStringLine info =
    rect
        [ x (String.fromFloat info.x)
        , y (String.fromFloat info.y)
        , width (String.fromFloat info.lineWidth)
        , height (String.fromFloat (info.height + info.lineWidth))
        ]
        []


renderFretLine : ImgInfo -> Svg msg
renderFretLine info =
    rect
        [ x (String.fromFloat info.x)
        , y (String.fromFloat info.y)
        , width (String.fromFloat (info.width + info.lineWidth))
        , height (String.fromFloat info.lineWidth)
        ]
        []



-- Svg.String helper and 'extensions'


x1 : Float -> Attribute msg
x1 value =
    attribute "x1" (String.fromFloat value)


x2 : Float -> Attribute msg
x2 value =
    attribute "x2" (String.fromFloat value)


y1 : Float -> Attribute msg
y1 value =
    attribute "y1" (String.fromFloat value)


y2 : Float -> Attribute msg
y2 value =
    attribute "y2" (String.fromFloat value)


textAnchor : String -> Attribute msg
textAnchor value =
    attribute "text-anchor" value


line : List (Attribute msg) -> List (Svg msg) -> Svg msg
line attribs children =
    node "line" (attribs ++ [ attribute "stroke" "black" ]) children



-- Decoders
-- MDN PageX define this as float
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageX


offsetX : Decoder Float
offsetX =
    field "offsetX" float



-- MDN PageY define this as float
--https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageY


offsetY : Decoder Float
offsetY =
    field "offsetY" float



-- See https://www.w3schools.com/jsref/obj_mouseevent.asp for properties. These get serialized to Json into elm


mouseXY : (Float -> Float -> msg) -> Decoder msg
mouseXY f =
    Json.Decode.map2 f offsetX offsetY
