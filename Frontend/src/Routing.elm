module Routing exposing (Model, Msg, init, onUrlChange, onUrlRequest, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, layout, map)
import Html exposing (Html)
import Pages.CreateChord exposing (initModel, page, update)
import Url exposing (Url)


type Model
    = CreateChord_Model Pages.CreateChord.Model
    | Unknown


type Msg
    = CreateChord_Msg Pages.CreateChord.Msg
    | Empty



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        CreateChord_Model m ->
            Browser.Document "BasicTitle" [ fromChordPage (page m) ]

        Unknown ->
            Browser.Document "BasicTitle" []


fromChordPage : Element Pages.CreateChord.Msg -> Html Msg
fromChordPage p =
    layout [] (map CreateChord_Msg p)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        CreateChord_Model m ->
            case msg of
                CreateChord_Msg cMsg ->
                    fromCreateChordUpdate (Pages.CreateChord.update cMsg m)

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


fromCreateChordUpdate : ( Pages.CreateChord.Model, Cmd Pages.CreateChord.Msg ) -> ( Model, Cmd Msg )
fromCreateChordUpdate ( model, msg ) =
    ( CreateChord_Model model, Cmd.map CreateChord_Msg msg )


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let
        a =
            Debug.log "onChangeUrl" url

        b =
            Debug.log "onChangeKey" key

        isCreate =
            url.path == "/createChord"
    in
    routing url.path


routing : String -> ( Model, Cmd Msg )
routing path =
    case path of
        "/createChord" ->
            ( CreateChord_Model initModel, Cmd.none )

        --             { chordList = [], status = None, svgModel = Just initSvgModel }, Cmd.none )
        _ ->
            ( Unknown, Nav.load "/createChord" )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlReq =
    let
        a =
            Debug.log "onUrlRequests" urlReq
    in
    Empty


onUrlChange : Url.Url -> Msg
onUrlChange url =
    let
        a =
            Debug.log "onChange" url
    in
    Empty
