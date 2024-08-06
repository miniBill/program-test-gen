module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict
import Lamdera
import Random
import Sha256
import Types exposing (..)
import Ui
import Url
import Url.Parser


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse Url.Parser.string url of
        Just sessionName ->
            ( LoadingSession
                { key = key
                , sessionName = SessionName sessionName
                }
            , Lamdera.sendToBackend (LoadSessionRequest (SessionName sessionName))
            )

        Nothing ->
            ( LoadingSession { key = key, sessionName = SessionName "" }
            , Random.generate
                GotRandomSessionName
                (Random.map
                    (\int -> String.fromInt int |> Sha256.sha224 |> String.left 16 |> SessionName)
                    (Random.int Random.minInt Random.maxInt)
                )
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        LoadingSession loading ->
            case msg of
                GotRandomSessionName sessionName ->
                    ( LoadingSession { loading | sessionName = sessionName }
                    , Cmd.batch
                        [ Lamdera.sendToBackend (LoadSessionRequest sessionName)
                        , Browser.Navigation.replaceUrl loading.key ("/" ++ sessionNameToString sessionName)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        LoadedSession loaded ->
            let
                ( newLoaded, cmd ) =
                    updateLoaded msg loaded
            in
            ( LoadedSession newLoaded, cmd )


sessionNameToString : SessionName -> String
sessionNameToString (SessionName sessionName) =
    sessionName


updateLoaded : FrontendMsg -> LoadedData -> ( LoadedData, Cmd FrontendMsg )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        PressedResetSession ->
            ( model, Lamdera.sendToBackend ResetSessionRequest )

        GotRandomSessionName _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        LoadSessionResponse events ->
            case model of
                LoadingSession loading ->
                    ( LoadedSession
                        { key = loading.key
                        , sessionName = loading.sessionName
                        , history = events
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SessionUpdate event ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = event :: loaded.history }, Cmd.none )

                LoadingSession _ ->
                    ( model, Cmd.none )

        ResetSession ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = [] }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


codegen : List Event -> String
codegen events =
    List.foldl
        (\event { code, indentation, clientCount } ->
            let
                indent =
                    String.repeat indentation "             "

                client =
                    if clientCount == 0 then
                        "client"

                    else
                        "client" ++ String.fromInt clientCount
            in
            case event.eventType of
                Connect { url, sessionId, windowWidth, windowHeight } ->
                    let
                        state =
                            if clientCount == 0 then
                                "state"

                            else
                                "state" ++ String.fromInt clientCount
                    in
                    { code =
                        code
                            ++ (indent ++ "    |> TF.connectFrontend\n")
                            ++ (indent ++ "        " ++ sessionId ++ "\n")
                            ++ (indent ++ "        " ++ url ++ "\n")
                            ++ (indent ++ "{ width = " ++ String.fromInt windowWidth ++ ", height = " ++ String.fromInt windowHeight ++ " }\n")
                            ++ (indent ++ "        \\( " ++ state ++ ", " ++ client ++ ") ->\n")
                            ++ (indent ++ "            " ++ state ++ "\n")
                    , indentation = indentation + 1
                    , clientCount = clientCount + 1
                    }

                KeyDown keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client
                            ++ ".keyDownEvent { keyCode = "
                            ++ String.fromInt keyEvent.keyCode
                            ++ " }\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyUp keyEvent ->
                    { code = code
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                --{ code =
                --    code
                --        ++ indent
                --        ++ "    |> "
                --        ++ client
                --        ++ ".keyDownEvent { keyCode = "
                --        ++ String.fromInt keyEvent.keyCode
                --        ++ " }\n"
                --, indentation = indentation
                --, clientCount = clientCount
                --}
                Click mouseEvent ->
                    { code =
                        case mouseEvent.targetId of
                            Just targetId ->
                                code
                                    ++ indent
                                    ++ "    |> "
                                    ++ client
                                    ++ ".clickButton "
                                    ++ targetId
                                    ++ "\n"

                            Nothing ->
                                code
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                Http httpEvent ->
                    { code = code, indentation = indentation, clientCount = clientCount }
        )
        { code = "TF.start config \"MyTest\"\n", indentation = 0, clientCount = 0 }
        (List.sortBy .timestamp events)
        |> .code


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Ui.layout
            []
            (case model of
                LoadingSession loading ->
                    Ui.text "Loading session..."

                LoadedSession loaded ->
                    eventsView loaded.history
            )
        ]
    }


eventsView : List Event -> Ui.Element msg
eventsView events =
    case events of
        [] ->
            Ui.text "No events have arrived"

        _ ->
            List.map
                (\event ->
                    (case event.eventType of
                        KeyDown keyEvent ->
                            keyEvent.key ++ " key down"

                        KeyUp keyEvent ->
                            keyEvent.key ++ " key up"

                        Click mouseEvent ->
                            "mouse click"

                        Http httpEvent ->
                            "http request"

                        Connect record ->
                            record.sessionId ++ " connected"
                    )
                        |> Ui.text
                )
                events
                |> Ui.column []
