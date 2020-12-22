module Routing exposing (Model, Msg, init, onUrlChange, onUrlRequest, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, layout, map)
import Html exposing (Html)
import Pages.ChordsOverview as ChordsOverview
import Pages.CreateChord as CreateChord
import Url exposing (Url)


type Model
    = CreateChord_Model CreateChord.Model
    | ChordOverview_Model ChordsOverview.Model
    | NotFound


type Msg
    = CreateChord_Msg CreateChord.Msg
    | ChordsOverview_Msg ChordsOverview.Msg
    | UrlRequest
    | UrlChange



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        CreateChord_Model m ->
            Browser.Document "Create Chord" [ fromChordPage (CreateChord.page m) ]

        ChordOverview_Model m ->
            Browser.Document "Create Chord" [ fromOverviewPage (ChordsOverview.page m) ]

        NotFound ->
            Browser.Document "Redirect" []


fromChordPage : Element CreateChord.Msg -> Html Msg
fromChordPage p =
    layout [] (map CreateChord_Msg p)


fromOverviewPage : Element ChordsOverview.Msg -> Html Msg
fromOverviewPage p =
    layout [] (map ChordsOverview_Msg p)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        CreateChord_Model m ->
            case msg of
                CreateChord_Msg cMsg ->
                    fromCreateChordUpdate (CreateChord.update cMsg m)

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


fromCreateChordUpdate : ( CreateChord.Model, Cmd CreateChord.Msg ) -> ( Model, Cmd Msg )
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
            ( CreateChord_Model CreateChord.initModel, Cmd.none )

        "/chordOverView" ->
            ( ChordOverview_Model ChordsOverview.initModel, Cmd.none )

        _ ->
            ( NotFound, Nav.load "/createChord" )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlReq =
    let
        a =
            Debug.log "onUrlRequests" urlReq
    in
    UrlRequest


onUrlChange : Url.Url -> Msg
onUrlChange url =
    let
        a =
            Debug.log "onChange" url
    in
    UrlChange
