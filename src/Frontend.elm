port module Frontend exposing (addEvent, app)

import Array exposing (Array)
import Array.Extra
import AssocSet
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import Html
import Icons
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Maybe.Extra
import Random
import Set
import Sha256
import Task
import Types exposing (..)
import Ui
import Ui.Anim
import Ui.Font
import Ui.Input
import Url
import Url.Parser


port copy_to_clipboard_to_js : String -> Cmd msg


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

        PressedSetEventVisibility index isHidden ->
            ( { model | history = Array.Extra.update index (\event -> { event | isHidden = isHidden }) model.history }
            , SetEventVisibilityRequest { index = index, isHidden = isHidden } |> Lamdera.sendToBackend
            )

        PressedCopyCode ->
            ( { model | copyCounter = model.copyCounter + 1 }
            , Array.toList model.history |> List.filter (\event -> not event.isHidden) |> codegen |> copy_to_clipboard_to_js
            )

        ElmUiMsg msg2 ->
            ( { model | elmUiState = Ui.Anim.update ElmUiMsg msg2 model.elmUiState }, Cmd.none )


addEvent : Event -> { a | history : Array Event } -> { a | history : Array Event }
addEvent event model =
    let
        event2 : Event
        event2 =
            { event
                | isHidden =
                    case event.eventType of
                        KeyUp keyEvent ->
                            shouldHideKeyEvent keyEvent

                        KeyDown keyEvent ->
                            shouldHideKeyEvent keyEvent

                        _ ->
                            False
            }
    in
    { model
        | history =
            case Array.get (Array.length model.history - 1) model.history of
                Just last ->
                    if event2.timestamp - last.timestamp < 0 then
                        Array.push event2 model.history
                            |> Array.toList
                            |> List.sortBy .timestamp
                            |> Array.fromList

                    else
                        Array.push event2 model.history

                Nothing ->
                    Array.push event2 model.history
    }


shouldHideKeyEvent : KeyEvent -> Bool
shouldHideKeyEvent keyEvent =
    (String.length keyEvent.key == 1 || keyEvent.key == "Shift" || keyEvent.key == "Backspace")
        && not keyEvent.altKey
        && not keyEvent.ctrlKey
        && not keyEvent.metaKey


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
                        , copyCounter = 0
                        , elmUiState = Ui.Anim.init
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SessionUpdate event ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession (addEvent event loaded)
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
    = Input2 { targetId : String, text : String }
    | Click2 MouseEvent
    | ClickLink2 LinkEvent
    | Connect2 ConnectEvent
    | KeyUp2 KeyEvent
    | KeyDown2 KeyEvent
    | PointerDown2 PointerEvent
    | PointerUp2 PointerEvent
    | PointerMove2 PointerEvent
    | PointerLeave2 PointerEvent
    | PointerCancel2 PointerEvent
    | PointerOver2 PointerEvent
    | PointerEnter2 PointerEvent
    | PointerOut2 PointerEvent
    | TouchStart2 TouchEvent
    | TouchCancel2 TouchEvent
    | TouchMove2 TouchEvent
    | TouchEnd2 TouchEvent
    | FromJsPort2 FromJsPortEvent
    | WindowResize2 WindowResizeEvent


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
                ( Input input, Just (Input2 _) ) ->
                    { previousEvent = Input2 input |> Just
                    , previousClientId = clientId
                    , rest = state.rest
                    }

                ( Input input, _ ) ->
                    { previousEvent = Input2 input |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( KeyDown keyDown, _ ) ->
                    { previousEvent = KeyDown2 keyDown |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( KeyUp keyUp, _ ) ->
                    { previousEvent = KeyUp2 keyUp |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerDown a, _ ) ->
                    { previousEvent = PointerDown2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerUp a, _ ) ->
                    { previousEvent = PointerUp2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerMove a, _ ) ->
                    { previousEvent = PointerMove2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerLeave a, _ ) ->
                    { previousEvent = PointerLeave2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerCancel a, _ ) ->
                    { previousEvent = PointerCancel2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerOver a, _ ) ->
                    { previousEvent = PointerOver2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerEnter a, _ ) ->
                    { previousEvent = PointerEnter2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( PointerOut a, _ ) ->
                    { previousEvent = PointerOut2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( TouchStart a, _ ) ->
                    { previousEvent = TouchStart2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( TouchCancel a, _ ) ->
                    { previousEvent = TouchCancel2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( TouchMove a, _ ) ->
                    { previousEvent = TouchMove2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( TouchEnd a, _ ) ->
                    { previousEvent = TouchEnd2 a |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( Connect connect, _ ) ->
                    { previousEvent = Connect2 connect |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( Click mouseEvent, _ ) ->
                    { previousEvent = Click2 mouseEvent |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( ClickLink linkEvent, _ ) ->
                    { previousEvent = ClickLink2 linkEvent |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( Http _, _ ) ->
                    state

                ( HttpLocal _, _ ) ->
                    state

                ( Paste pasteEvent, Just (Input2 input) ) ->
                    case pasteEvent.targetId of
                        Just targetId ->
                            if targetId == input.targetId && state.previousClientId == clientId then
                                { state
                                    | previousEvent = Input2 { input | text = input.text ++ pasteEvent.text } |> Just
                                    , previousClientId = clientId
                                }

                            else
                                { previousEvent = Input2 { targetId = targetId, text = pasteEvent.text } |> Just
                                , previousClientId = clientId
                                , rest = [ toEvent clientId (Input2 input) ] ++ state.rest
                                }

                        Nothing ->
                            state

                ( Paste pasteEvent, _ ) ->
                    case pasteEvent.targetId of
                        Just targetId ->
                            { previousEvent = Input2 { targetId = targetId, text = pasteEvent.text } |> Just
                            , previousClientId = clientId
                            , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                            }

                        Nothing ->
                            state

                ( ResetBackend, _ ) ->
                    state

                ( FromJsPort fromJsPort, _ ) ->
                    { previousEvent = FromJsPort2 fromJsPort |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }

                ( WindowResize resizeEvent, _ ) ->
                    { previousEvent = WindowResize2 resizeEvent |> Just
                    , previousClientId = clientId
                    , rest = Maybe.Extra.toList (Maybe.map (toEvent clientId) state.previousEvent) ++ state.rest
                    }
        )
        { previousClientId = "", previousEvent = Nothing, rest = [] }
        events
        |> (\state -> Maybe.Extra.toList (Maybe.map (toEvent state.previousClientId) state.previousEvent) ++ state.rest)
        |> List.reverse


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


codegen : List Event -> String
codegen events =
    let
        clients : List ClientId
        clients =
            List.map .clientId events |> AssocSet.fromList |> AssocSet.toList

        tests =
            List.Extra.groupWhile (\a _ -> a.eventType /= ResetBackend) events
                |> List.filter (\( _, rest ) -> not (List.isEmpty rest))
                |> List.indexedMap (\index ( head, rest ) -> testCode clients index (head :: rest))
                |> String.join "\n    ,"
    in
    setupCode events ++ tests ++ "\n    ]"


testCode : List ClientId -> Int -> List Event -> String
testCode clients testIndex events =
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

                WindowResize2 resizeEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client
                            ++ ".resizeWindow { width = "
                            ++ String.fromInt resizeEvent.width
                            ++ ", height = "
                            ++ String.fromInt resizeEvent.height
                            ++ " }\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyDown2 keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client
                            ++ ".keyDown "
                            ++ targetIdFunc keyEvent.targetId
                            ++ " { key = \""
                            ++ keyEvent.key
                            ++ "\", shift = "
                            ++ boolToString keyEvent.shiftKey
                            ++ ", alt = "
                            ++ boolToString keyEvent.altKey
                            ++ ", ctrl = "
                            ++ boolToString keyEvent.ctrlKey
                            ++ ", meta = "
                            ++ boolToString keyEvent.metaKey
                            ++ " }\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyUp2 keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client
                            ++ ".keyUp "
                            ++ targetIdFunc keyEvent.targetId
                            ++ " { key = \""
                            ++ keyEvent.key
                            ++ "\", shift = "
                            ++ boolToString keyEvent.shiftKey
                            ++ ", alt = "
                            ++ boolToString keyEvent.altKey
                            ++ ", ctrl = "
                            ++ boolToString keyEvent.ctrlKey
                            ++ ", meta = "
                            ++ boolToString keyEvent.metaKey
                            ++ " }\n"
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

                Input2 { targetId, text } ->
                    { code = code ++ indent ++ "    |> " ++ client ++ ".inputText " ++ targetIdFunc targetId ++ " \"" ++ text ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                FromJsPort2 _ ->
                    { code = code
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerDown2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerDown" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerUp2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerUp" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerMove2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerMove" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerLeave2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerLeave" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerCancel2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerCancel" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerOver2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerOver" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerEnter2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerEnter" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerOut2 a ->
                    { code = code ++ indent ++ pointerCodegen "pointerOut" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchStart2 a ->
                    { code = code ++ indent ++ touchCodegen "touchStart" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchCancel2 a ->
                    { code = code ++ indent ++ touchCodegen "touchCancel" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchMove2 a ->
                    { code = code ++ indent ++ touchCodegen "touchMove" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchEnd2 a ->
                    { code = code ++ indent ++ touchCodegen "touchEnd" client a
                    , indentation = indentation
                    , clientCount = clientCount
                    }
        )
        { code = " TF.start (config httpData) \"test " ++ String.fromInt testIndex ++ "\"\n"
        , indentation = 0
        , clientCount = 0
        }
        (eventsToEvent2 events)
        |> (\{ code, indentation } -> code ++ "        " ++ String.repeat indentation ")")


touchCodegen : String -> String -> TouchEvent -> String
touchCodegen funcName client a =
    let
        touchToString : Touch -> String
        touchToString touch =
            "{ id = "
                ++ String.fromInt touch.identifier
                ++ ", screenPos = ("
                ++ String.fromFloat touch.screenX
                ++ ", "
                ++ String.fromFloat touch.screenY
                ++ "), clientPos = ("
                ++ String.fromFloat touch.clientX
                ++ ", "
                ++ String.fromFloat touch.clientY
                ++ "), pagePos = ("
                ++ String.fromFloat touch.pageX
                ++ ", "
                ++ String.fromFloat touch.pageY
                ++ ") }"
    in
    "    |> "
        ++ client
        ++ "."
        ++ funcName
        ++ " "
        ++ targetIdFunc a.targetId
        ++ " { targetTouches = [ "
        ++ String.join ", " (List.map touchToString a.targetTouches)
        ++ " ], changedTouches = [ "
        ++ String.join ", " (List.map touchToString a.targetTouches)
        ++ " ] }\n"


pointerCodegen : String -> String -> PointerEvent -> String
pointerCodegen funcName client a =
    let
        options : List String
        options =
            List.filterMap
                (\( name, x, y ) ->
                    if x == a.offsetX && y == a.offsetY then
                        Nothing

                    else
                        Just (name ++ " " ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                )
                [ ( "ScreenPos", a.screenX, a.screenY )
                , ( "PagePos", a.pageX, a.pageY )
                , ( "ClientPos", a.clientX, a.clientY )
                ]
                ++ List.filterMap
                    identity
                    [ case a.button of
                        1 ->
                            Just "PointerButton MainButton"

                        2 ->
                            Just "PointerButton MiddleButton"

                        3 ->
                            Just "PointerButton SecondButton"

                        4 ->
                            Just "PointerButton BackButton"

                        5 ->
                            Just "PointerButton ForwardButton"

                        _ ->
                            Nothing
                    , if a.altKey then
                        Just "AltHeld"

                      else
                        Nothing
                    , if a.shiftKey then
                        Just "ShiftHeld"

                      else
                        Nothing
                    , if a.ctrlKey then
                        Just "CtrlHeld"

                      else
                        Nothing
                    , if a.metaKey then
                        Just "MetaHeld"

                      else
                        Nothing
                    , if a.pointerId == 0 then
                        Nothing

                      else
                        Just ("PointerId " ++ String.fromInt a.pointerId)
                    , if a.isPrimary then
                        Nothing

                      else
                        Just "IsNotPrimary"
                    ]
    in
    "    |> "
        ++ client
        ++ "."
        ++ funcName
        ++ " "
        ++ targetIdFunc a.targetId
        ++ " { offsetPos = ("
        ++ String.fromFloat a.offsetX
        ++ ","
        ++ String.fromFloat a.offsetY
        ++ ") } [ "
        ++ String.join ", " options
        ++ " ]\n"


setupCode : List Event -> String
setupCode events =
    let
        httpRequests : String
        httpRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        Http http ->
                            { http | url = dropPrefix "http://localhost:8001/" http.url } |> Just

                        _ ->
                            Nothing
                )
                events
                |> List.Extra.uniqueBy (\a -> ( a.method, a.url ))
                |> List.map (\http -> "(\"" ++ http.method ++ "_" ++ http.url ++ "\", \"" ++ http.filepath ++ "\")")
                |> (\a -> a ++ localRequests)
                |> String.join "\n    , "

        localRequests : List String
        localRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        HttpLocal { filepath } ->
                            Just filepath

                        _ ->
                            Nothing
                )
                events
                |> Set.fromList
                |> Set.toList
                |> List.map (\filepath -> "(\"GET_" ++ filepath ++ "\", \"/public" ++ filepath ++ "\")")

        portRequests : String
        portRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        FromJsPort fromJsPort ->
                            case fromJsPort.triggeredFromPort of
                                Just trigger ->
                                    Just { triggeredFromPort = trigger, port_ = fromJsPort.port_, data = fromJsPort.data }

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing
                )
                events
                |> List.Extra.uniqueBy (\a -> a.triggeredFromPort)
                |> List.map
                    (\fromJsPort ->
                        "(\""
                            ++ fromJsPort.triggeredFromPort
                            ++ "\", (\""
                            ++ fromJsPort.port_
                            ++ "\", stringToJson "
                            ++ (if String.contains "\"" fromJsPort.data then
                                    "\"\"\"" ++ fromJsPort.data ++ "\"\"\""

                                else
                                    "\"" ++ fromJsPort.data ++ "\""
                               )
                            ++ "))"
                    )
                |> String.join "\n    , "
    in
    """module MyTests exposing (main, setup, tests)

import Effect.Browser.Dom as Dom
import Effect.Test as TF exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..), PointerOptions(..))
import Frontend
import Backend
import Url exposing (Url)
import Bytes exposing (Bytes)
import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
import Dict exposing (Dict)
import Effect.Lamdera
import Json.Decode
import Json.Decode
import Json.Encode

setup : TF.ViewerWith (List (TF.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    TF.viewerWith tests
        |> TF.addBytesFiles (Dict.values httpRequests)


main : Program () (TF.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (TF.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    TF.startViewer setup


domain : Url
domain = { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


config : Dict String Bytes -> TF.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
config httpData =
    TF.Config Frontend.app_ Backend.app_ (handleHttpRequests httpData) handlePortToJs handleFileRequest handleMultiFileUpload domain


stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


handlePortToJs : { currentRequest : TF.PortToJs, data : TF.Data FrontendModel BackendModel } -> Maybe ( String, Json.Decode.Value )
handlePortToJs { currentRequest } =
    Dict.get currentRequest.portName portRequests


{-| Please don't modify or rename this function -}
portRequests : Dict String (String, Json.Encode.Value)
portRequests =
    [ """
        ++ portRequests
        ++ """
    ]
        |> Dict.fromList


handleFileRequest : { data : TF.Data frontendModel backendModel, mimeTypes : List String } -> FileUpload
handleFileRequest _ =
    UnhandledFileUpload


{-| Please don't modify or rename this function -}
httpRequests : Dict String String
httpRequests =
    [ """
        ++ httpRequests
        ++ """
    ]
        |> Dict.fromList


handleHttpRequests : Dict String Bytes -> { currentRequest : HttpRequest, data : TF.Data FrontendModel BackendModel } -> HttpResponse
handleHttpRequests httpData { currentRequest } =
    case Dict.get (currentRequest.method ++ "_" ++ currentRequest.url) httpRequests of
        Just filepath ->
            case Dict.get filepath httpData of
                Just data ->
                    BytesHttpResponse { url = currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty } data

                Nothing ->
                    UnhandledHttpRequest

        Nothing ->
            UnhandledHttpRequest


handleMultiFileUpload : { data : TF.Data frontendModel backendModel, mimeTypes : List String } -> MultipleFilesUpload
handleMultiFileUpload _ =
    UnhandledMultiFileUpload


{-| You can change parts of this function represented with `...`.
The rest needs to remain unchanged in order for the test generator to be able to add new tests.

    tests : ... -> List (TF.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
    tests ... =
        [ ...
        ]
-}
tests : Dict String Bytes -> List (TF.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests httpData =
    ["""


targetIdFunc : String -> String
targetIdFunc id =
    "(Dom.id \"" ++ id ++ "\")"


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ case model of
            LoadingSession _ ->
                Html.text "Loading session..."

            LoadedSession loaded ->
                let
                    eventsList : List Event
                    eventsList =
                        Array.toList loaded.history
                in
                Ui.Anim.layout
                    { options = []
                    , breakpoints = Nothing
                    , toMsg = ElmUiMsg
                    }
                    loaded.elmUiState
                    [ Ui.Font.family [ Ui.Font.sansSerif ], Ui.padding 16, Ui.height Ui.fill ]
                    (Ui.row
                        [ Ui.height Ui.fill ]
                        [ Ui.column
                            [ Ui.height Ui.fill, Ui.width Ui.shrink, Ui.spacing 8 ]
                            [ button PressedResetSession [ Ui.text "Reset" ]
                            , eventsView eventsList
                            ]
                        , Ui.column
                            [ Ui.height Ui.fill, Ui.spacing 8 ]
                            [ Ui.row
                                [ Ui.spacing 16 ]
                                [ button
                                    PressedCopyCode
                                    [ Icons.copy
                                    , if loaded.copyCounter > 0 then
                                        Ui.text "Copied!"

                                      else
                                        Ui.text " Copy"
                                    ]
                                , Ui.el [ Ui.Font.size 14 ] (Ui.text "Press \"Reset Backend\" in lamdera live to start a new test")
                                ]
                            , List.filter (\event -> not event.isHidden) eventsList
                                |> codegen
                                |> Ui.text
                                |> Ui.el
                                    [ Ui.Font.family [ Ui.Font.monospace ]
                                    , Ui.Font.size 12
                                    , Ui.Font.exactWhitespace
                                    , Ui.scrollable
                                    ]
                            ]
                        ]
                    )
        ]
    }


button : msg -> List (Ui.Element msg) -> Ui.Element msg
button msg content =
    Ui.row
        [ Ui.Input.button msg
        , Ui.border 1
        , Ui.borderColor (Ui.rgb 100 100 100)
        , Ui.background (Ui.rgb 240 240 240)
        , Ui.width Ui.shrink
        , Ui.padding 8
        , Ui.spacing 4
        , Ui.rounded 4
        ]
        content


eventsView : List Event -> Ui.Element FrontendMsg
eventsView events =
    case events of
        [] ->
            Ui.el [ Ui.width (Ui.px 300) ] (Ui.text "No events have arrived")

        _ ->
            List.indexedMap
                (\index event ->
                    Ui.row
                        [ Ui.Input.button (PressedSetEventVisibility index (not event.isHidden))
                        , Ui.spacing 4
                        , Ui.Font.color
                            (if event.isHidden then
                                Ui.rgb 120 120 120

                             else
                                Ui.rgb 0 0 0
                            )
                        ]
                        [ if event.isHidden then
                            Icons.eyeClosed

                          else
                            Icons.eye
                        , (case event.eventType of
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

                            Http _ ->
                                "http request"

                            Connect record ->
                                record.sessionId ++ " connected"

                            ClickLink linkEvent ->
                                "clicked link to " ++ linkEvent.path

                            Paste pasteEvent ->
                                "pasted text " ++ ellipsis pasteEvent.text

                            Input inputEvent ->
                                "text input " ++ ellipsis inputEvent.text

                            ResetBackend ->
                                "test ended"

                            FromJsPort fromJsPortEvent ->
                                "port " ++ fromJsPortEvent.port_ ++ " " ++ ellipsis fromJsPortEvent.data

                            HttpLocal { filepath } ->
                                "loaded " ++ filepath

                            WindowResize { width, height } ->
                                "Window resized w:" ++ String.fromInt width ++ " h:" ++ String.fromInt height

                            PointerDown _ ->
                                "Pointer down"

                            PointerUp _ ->
                                "Pointer up"

                            PointerMove pointerEvent ->
                                let
                                    xyToString x y =
                                        String.fromFloat x ++ "," ++ String.fromFloat y
                                in
                                "Pointer move (client "
                                    ++ xyToString pointerEvent.clientX pointerEvent.clientY
                                    ++ ") (offset "
                                    ++ xyToString pointerEvent.offsetX pointerEvent.offsetY
                                    ++ ") (screen "
                                    ++ xyToString pointerEvent.screenX pointerEvent.screenX
                                    ++ ") (page "
                                    ++ xyToString pointerEvent.pageX pointerEvent.pageY
                                    ++ ")"

                            PointerLeave _ ->
                                "Pointer leave"

                            PointerCancel _ ->
                                "Pointer cancel"

                            PointerOver _ ->
                                "Pointer over"

                            PointerEnter _ ->
                                "Pointer enter"

                            PointerOut _ ->
                                "Pointer out"

                            TouchStart _ ->
                                "Touch start"

                            TouchCancel _ ->
                                "Touch cancel"

                            TouchMove _ ->
                                "Touch move"

                            TouchEnd _ ->
                                "Touch end"
                          )
                            |> Ui.text
                        ]
                )
                events
                |> Ui.column
                    [ Ui.width (Ui.px 300)
                    , Ui.scrollable
                    , Ui.height Ui.fill
                    , Ui.Font.size 14
                    , Ui.id eventsListContainer
                    ]


ellipsis : String -> String
ellipsis text =
    if String.length text < 10 then
        text

    else
        String.left 7 text ++ "..."
