module SvgStrumming.RenderNotes exposing (RenderFunc, RenderNote, beatWidth, getRenderF, renderNotes)

import Svg exposing (Attribute, Svg, circle, node, rect, svg)
import Svg.Attributes as SA exposing (..)
import SvgStrumming.SvgStrumming exposing (..)


beatWidth : Float
beatWidth =
    noteWidth * 2


type alias RenderFunc msg =
    List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)


type alias RenderNote msg =
    { renderF : RenderFunc msg
    , isCurrent : Bool
    , beatsDuration : Float
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

        Rest d ->
            case d of
                Whole ->
                    renderWholeRest

                Half ->
                    renderHalfRest

                Quater ->
                    renderQuaterRest

                Eighth ->
                    renderEighthRest


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



-- Render Notes and rests


renderDefaultRest : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderDefaultRest attribs info pos =
    [ rect
        ([ width (String.fromFloat 24)
         , height (String.fromFloat 16)
         , x (String.fromFloat pos.x)
         , y (String.fromFloat (pos.y - 6))
         ]
            ++ attribs
        )
        []
    ]


renderWholeRest : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderWholeRest attribs info pos =
    let
        def =
            renderDefaultRest attribs info pos

        extra =
            [ rect
                ([ width (String.fromFloat noteWidth)
                 , height (String.fromFloat 5)
                 , x (String.fromFloat (pos.x - 9))
                 , y (String.fromFloat (pos.y - 6))
                 ]
                    ++ attribs
                )
                []
            ]
    in
    def ++ extra


renderHalfRest : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderHalfRest attribs info pos =
    let
        def =
            renderDefaultRest attribs info pos

        extra =
            [ rect
                ([ width (String.fromFloat noteWidth)
                 , height (String.fromFloat 5)
                 , x (String.fromFloat (pos.x - 9))
                 , y (String.fromFloat (pos.y + 6))
                 ]
                    ++ attribs
                )
                []
            ]
    in
    def ++ extra


renderQuaterRest : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderQuaterRest attribs _ pos =
    let
        scaleString =
            "scale(0.6) "

        newPos =
            posAdd pos { x = noteWidth / 2, y = -15 }

        path =
            Svg.path
                (attribs
                    ++ [ SA.d <| quaterRestString
                       ]
                )
                []
    in
    [ translate newPos (node "g" [ SA.transform scaleString ] [ path ]) ]


quaterRestString : String
quaterRestString =
    "M 0 0 c -6.84377,8.15627 -10.26565,14.25001 -10.26562,18.28125 -3e-5,3.89063 3.25778,9.72656 9.77343,17.50781 l -2.03906,2.88282 c -3.28128,-1.92189 -6.09377,-2.88283 -8.4375,-2.88282 -3.04689,-10e-6 -4.57033,1.82811 -4.57031,5.48438 -2e-5,3.74998 1.66404,7.47654 4.99219,11.17969 l -1.82813,2.74218 c -9.23438,-6.84377 -13.85157,-12.93751 -13.85156,-18.28125 -1e-5,-2.71876 0.93749,-4.875 2.8125,-6.46875 1.73436,-1.5 3.98436,-2.25 6.75,-2.25 1.78123,0 3.74998,0.46875 5.90625,1.40625 l -14.13281,-18.77343 c 6.70311,-5.90624 10.05467,-11.24998 10.05468,-16.03125 -10e-6,-3.79685 -2.27345,-8.57809 -6.82031,-14.34375 l 5.625,0 16.03125,19.54687"


renderEighthRest : List (Attribute msg) -> ImgInfo -> Pos -> List (Svg msg)
renderEighthRest attribs _ pos =
    let
        scaleString =
            "scale(1) "

        newPos =
            posAdd pos { x = 0, y = -25 }

        path =
            Svg.path
                (attribs
                    ++ [ SA.d <| eighthRestString
                       ]
                )
                []
    in
    [ translate newPos (node "g" [ SA.transform scaleString ] [ path ]) ]


eighthRestString : String
eighthRestString =
    "m 0,0 c -2.46585,0.46464 -4.35316,2.16673 -5.20671,4.51834 -0.18493,0.75861 -0.18493,0.94351 -0.18493,1.9818 0,1.4271 0.0901,2.18572 0.75871,3.31409 0.94364,1.89173 2.92578,3.4089 5.18773,3.96362 2.37097,0.66851 6.33527,0.0949 10.87335,-1.40812 l 1.1286,-0.38876 -5.57659,15.40882 -5.48172,15.38984 c 0,0 0.18496,0.0948 0.4837,0.29869 0.5548,0.36983 1.49844,0.64957 2.16707,0.64957 1.12859,0 2.55593,-0.64957 2.74086,-1.22322 0,-0.18493 2.64603,-9.16945 5.8516,-19.83708 l 5.66668,-19.55738 -0.18496,-0.27496 c -0.45993,-0.57369 -1.40361,-0.75862 -1.98214,-0.29872 -0.18493,0.18492 -0.47893,0.57368 -0.66385,0.85344 -0.85358,1.42708 -3.02065,3.96361 -4.14925,4.90709 -1.03851,0.85341 -1.61226,0.94352 -2.55594,0.57369 -0.85354,-0.46464 -1.1333,-0.94351 -1.7071,-3.49897 -0.5548,-2.53653 -1.21868,-3.68866 -2.64602,-4.63214 -1.31827,-0.84869 -3.02065,-1.1284 -4.5191,-0.73964 z"


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


translate : Pos -> Svg msg -> Svg msg
translate pos el =
    let
        translateString =
            "translate("
                ++ String.fromFloat pos.x
                ++ ","
                ++ String.fromFloat pos.y
                ++ ")"
    in
    node "g" [ SA.transform translateString ] [ el ]



--HELPERS


posAdd : Pos -> Pos -> Pos
posAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


posAddY : Pos -> Float -> Pos
posAddY p1 y =
    { p1 | y = p1.y + y }


posAddX : Pos -> Float -> Pos
posAddX p1 x =
    { p1 | x = p1.x + x }
