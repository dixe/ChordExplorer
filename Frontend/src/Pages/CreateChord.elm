module Pages.CreateChord exposing (Model, Msg, initModel, page, update)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File.Download as Download
import Layout.Helper as LH exposing (..)
import SvgChord.Logic exposing (initSvgModel, modelClicked)
import SvgChord.Render exposing (createChordView, getSvgString)
import SvgChord.Types exposing (SvgModel)


type Msg
    = SvgClickPos Float Float


type alias Model =
    SvgModel


initModel : Model
initModel =
    initSvgModel


page : Model -> Element Msg
page model =
    column [ width fill, spacing 10 ]
        [ LH.header
        , LH.spacerLine
        , createChordView model SvgClickPos
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SvgClickPos x y ->
            ( updateSvgModelClick x y model, Cmd.none )


updateSvgModelClick : Float -> Float -> SvgModel -> SvgModel
updateSvgModelClick x y model =
    modelClicked { x = x, y = y } model


saveSvg : String -> Cmd Msg
saveSvg content =
    Download.string "chord.svg" "application/svg" content
