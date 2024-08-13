module Frontend exposing (..)

import Array
import AssocSet
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Random
import Sha256
import Task
import Types exposing (..)
import Ui
import Ui.Font
import Ui.Input
import Url
import Url.Parser


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
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

        ScrolledToBottom ->
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
                    ( LoadedSession { loaded | history = Array.push event loaded.history }
                    , Browser.Dom.setViewportOf eventsListContainer 0 99999
                        |> Task.attempt (\_ -> ScrolledToBottom)
                    )

                LoadingSession _ ->
                    ( model, Cmd.none )

        ResetSession ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = Array.empty }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


eventsListContainer : String
eventsListContainer =
    "eventsListContainer"


type EventType2
    = InputString { targetId : String, text : String }
    | Click2 MouseEvent
    | ClickLink2 LinkEvent
    | Connect2 { url : String, sessionId : SessionId, windowWidth : Int, windowHeight : Int }
    | KeyUp2 KeyEvent
    | KeyDown2 KeyEvent


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just a ->
            [ a ]

        Nothing ->
            []


addKeyToInputString : KeyEvent -> String -> String
addKeyToInputString keyEvent string =
    if keyEvent.key == "Backspace" then
        String.dropRight 1 string

    else if String.length keyEvent.key > 2 then
        string

    else
        string ++ keyEvent.key


eventsToEvent2 : List Event -> List { eventType : EventType2, clientId : ClientId }
eventsToEvent2 events =
    let
        toEvent clientId eventType2 =
            { clientId = clientId
            , eventType = eventType2
            }
    in
    List.foldl
        (\{ clientId, eventType } state ->
            case ( eventType, state.previousEvent ) of
                ( KeyDown keyDown, Just (InputString input) ) ->
                    case keyDown.targetId of
                        Just targetId ->
                            if targetId == input.targetId && state.previousClientId == clientId then
                                { state
                                    | previousEvent = InputString { input | text = addKeyToInputString keyDown input.text } |> Just
                                    , previousClientId = clientId
                                }

                            else
                                { previousEvent = InputString { targetId = targetId, text = keyDown.key } |> Just
                                , previousClientId = clientId
                                , rest = [ toEvent clientId (InputString input) ] ++ state.rest
                                }

                        Nothing ->
                            { previousEvent = KeyDown2 keyDown |> Just
                            , previousClientId = clientId
                            , rest = [ toEvent clientId (InputString input) ] ++ state.rest
                            }

                ( KeyDown keyDown, _ ) ->
                    { previousEvent =
                        case keyDown.targetId of
                            Just targetId ->
                                InputString { targetId = targetId, text = addKeyToInputString keyDown "" } |> Just

                            Nothing ->
                                KeyDown2 keyDown |> Just
                    , previousClientId = clientId
                    , rest = maybeToList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( KeyUp _, Just (InputString _) ) ->
                    state

                ( KeyUp keyUp, _ ) ->
                    { previousEvent = KeyUp2 keyUp |> Just
                    , previousClientId = clientId
                    , rest = maybeToList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( Connect connect, _ ) ->
                    { previousEvent = Connect2 connect |> Just
                    , previousClientId = clientId
                    , rest = maybeToList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( Click mouseEvent, _ ) ->
                    { previousEvent = Click2 mouseEvent |> Just
                    , previousClientId = clientId
                    , rest = maybeToList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( ClickLink linkEvent, _ ) ->
                    { previousEvent = ClickLink2 linkEvent |> Just
                    , previousClientId = clientId
                    , rest = maybeToList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( Http httpEvent, _ ) ->
                    state
        )
        { previousClientId = "", previousEvent = Nothing, rest = [] }
        events
        |> (\state -> maybeToList (Maybe.map (toEvent state.previousClientId) state.previousEvent) ++ state.rest)
        |> List.reverse


dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


codegen : List Event -> String
codegen events =
    let
        clients : List ClientId
        clients =
            List.map .clientId events |> AssocSet.fromList |> AssocSet.toList

        httpRequests : String
        httpRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        Http http ->
                            http
                                |> Just

                        _ ->
                            Nothing
                )
                events
                |> List.Extra.uniqueBy (\a -> ( a.method, a.url ))
                |> List.map
                    (\http ->
                        ("        (\"" ++ http.method ++ "\", \"" ++ dropPrefix "http://localhost:8001/" http.url ++ "\") ->\n")
                            ++ "            Dict.get \""
                            ++ http.responseHash
                            ++ "\" httpData |> Maybe.map (BytesHttpResponse { url = currentRequest.url, statusCode = 200, statusText = \"OK\", headers = Dict.empty }) |> Maybe.withDefault UnhandledHttpRequest\n\n"
                    )
                |> String.concat
    in
    List.foldl
        (\event { code, indentation, clientCount } ->
            let
                indent =
                    String.repeat indentation "             "

                client =
                    case List.Extra.findIndex (\a -> a == event.clientId) clients of
                        Just index ->
                            "client" ++ String.fromInt (index + 1)

                        Nothing ->
                            "client"
            in
            case event.eventType of
                Connect2 { url, sessionId, windowWidth, windowHeight } ->
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
                            ++ (indent ++ "        (Effect.Lamdera.sessionIdFromString \"" ++ sessionId ++ "\")\n")
                            ++ (indent ++ "        (Url.fromString \"" ++ url ++ "\" |> Maybe.withDefault domain)\n")
                            ++ (indent ++ "        { width = " ++ String.fromInt windowWidth ++ ", height = " ++ String.fromInt windowHeight ++ " }\n")
                            ++ (indent ++ "        (\\( " ++ state ++ ", " ++ client ++ ") ->\n")
                            ++ (indent ++ "            " ++ state ++ "\n")
                    , indentation = indentation + 1
                    , clientCount = clientCount + 1
                    }

                KeyDown2 keyEvent ->
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

                KeyUp2 keyEvent ->
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
                Click2 mouseEvent ->
                    { code =
                        case mouseEvent.targetId of
                            Just targetId ->
                                code
                                    ++ indent
                                    ++ "    |> "
                                    ++ client
                                    ++ ".clickButton "
                                    ++ targetIdFunc targetId
                                    ++ "\n"

                            Nothing ->
                                code
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                ClickLink2 mouseEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client
                            ++ ".clickLink \""
                            ++ mouseEvent.path
                            ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                InputString { targetId, text } ->
                    { code = code ++ indent ++ "    |> " ++ client ++ ".inputText " ++ targetIdFunc targetId ++ " \"" ++ text ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }
        )
        { code =
            """import Effect.Browser.Dom as Dom
import Effect.Test as TF exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..))
import Frontend
import Backend
import Url
import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
import Dict exposing (Dict)

domain : Url
domain = { protocol = Url.Http, host = "localhost", port_ = Just 8000, path : "", query = Nothing, fragment = Nothing }


config : Dict String Bytes -> TF.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel"
config httpData =
    TF.Config Frontend.app Backend.app (handleHttpRequests httpData) handlePortToJs handleFileRequest handleMultiFileUpload domain


handlePortToJs : { currentRequest : TF.PortToJs, data : TF.Data FrontendModel BackendModel } -> Maybe ( String, Json.Decode.Value )
handlePortToJs { currentRequest } =
    Nothing


handleFileRequest : { data : TF.Data frontendModel backendModel, mimeTypes : List String } -> FileUpload
handleFileRequest _ =
    UnhandledFileUpload


handleHttpRequests : Dict String Bytes -> { currentRequest : HttpRequest, data : TF.Data FrontendModel BackendModel } -> HttpResponse
handleHttpRequests httpData { currentRequest } =
    case (currentRequest.method, currentRequest.url) of
"""
                ++ httpRequests
                ++ """        _ ->
            UnhandledHttpRequest


handleMultiFileUpload : { data : TF.Data frontendModel backendModel, mimeTypes : List String } -> MultipleFilesUpload
handleMultiFileUpload _ =
    UnhandledMultiFileUpload


tests : Dict String Bytes -> List (TF.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests httpData =
    [ TF.start (config httpData) "MyTest"
    """
        , indentation = 0
        , clientCount = 0
        }
        (eventsToEvent2 events)
        |> (\{ code, indentation } -> code ++ "        " ++ String.repeat indentation ")" ++ "\n    ]")


targetIdFunc : String -> String
targetIdFunc id =
    "(Dom.id \"" ++ id ++ "\")"


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Ui.layout
            [ Ui.Font.family [ Ui.Font.sansSerif ], Ui.padding 16, Ui.height Ui.fill ]
            (case model of
                LoadingSession loading ->
                    Ui.text "Loading session..."

                LoadedSession loaded ->
                    let
                        eventsList : List Event
                        eventsList =
                            Array.toList loaded.history
                    in
                    Ui.row
                        [ Ui.height Ui.fill ]
                        [ Ui.column
                            [ Ui.height Ui.fill, Ui.width Ui.shrink, Ui.spacing 8 ]
                            [ Ui.el
                                [ Ui.Input.button PressedResetSession
                                , Ui.border 1
                                , Ui.borderColor (Ui.rgb 100 100 100)
                                , Ui.background (Ui.rgb 240 240 240)
                                , Ui.width Ui.shrink
                                , Ui.padding 8
                                ]
                                (Ui.text "Reset")
                            , eventsView eventsList
                            ]
                        , codegen eventsList
                            |> Ui.text
                            |> Ui.el
                                [ Ui.Font.family [ Ui.Font.monospace ]
                                , Ui.Font.size 12
                                , Ui.Font.exactWhitespace
                                , Ui.alignTop
                                ]
                        ]
            )
        ]
    }


eventsView : List Event -> Ui.Element msg
eventsView events =
    case events of
        [] ->
            Ui.el [ Ui.width (Ui.px 300) ] (Ui.text "No events have arrived")

        _ ->
            List.map
                (\event ->
                    (case event.eventType of
                        KeyDown keyEvent ->
                            keyEvent.key ++ " key down"

                        KeyUp keyEvent ->
                            keyEvent.key ++ " key up"

                        Click mouseEvent ->
                            case mouseEvent.targetId of
                                Just id ->
                                    "clicked on " ++ id

                                Nothing ->
                                    "clicked on nothing"

                        Http httpEvent ->
                            "http request"

                        Connect record ->
                            record.sessionId ++ " connected"

                        ClickLink linkEvent ->
                            "clicked link to " ++ linkEvent.path
                    )
                        |> Ui.text
                )
                events
                |> Ui.column
                    [ Ui.width (Ui.px 300)
                    , Ui.scrollable
                    , Ui.height Ui.fill
                    , Ui.Font.size 14
                    , Ui.id eventsListContainer
                    ]
