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
import Ui.Prose
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
            , Array.toList model.history
                |> List.filter (\event -> not event.isHidden)
                |> codegen model
                |> copy_to_clipboard_to_js
            )

        ElmUiMsg msg2 ->
            ( { model | elmUiState = Ui.Anim.update ElmUiMsg msg2 model.elmUiState }, Cmd.none )

        ToggledIncludeScreenPos bool ->
            ( { model | includeScreenPos = bool }
            , SetIncludeScreenPageClientPos
                { includeScreenPos = bool
                , includePagePos = model.includePagePos
                , includeClientPos = model.includeClientPos
                }
                |> Lamdera.sendToBackend
            )

        ToggledIncludeClientPos bool ->
            ( { model | includeClientPos = bool }
            , SetIncludeScreenPageClientPos
                { includeScreenPos = model.includeScreenPos
                , includePagePos = model.includePagePos
                , includeClientPos = bool
                }
                |> Lamdera.sendToBackend
            )

        ToggledIncludePagePos bool ->
            ( { model | includePagePos = bool }
            , SetIncludeScreenPageClientPos
                { includeScreenPos = model.includeScreenPos
                , includePagePos = bool
                , includeClientPos = model.includeClientPos
                }
                |> Lamdera.sendToBackend
            )


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
                        , history = events.history
                        , copyCounter = 0
                        , elmUiState = Ui.Anim.init
                        , includeScreenPos = events.includeScreenPos
                        , includePagePos = events.includePagePos
                        , includeClientPos = events.includeClientPos
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
    = Input2 ClientId { targetId : String, text : String }
    | Click2 ClientId { targetId : String }
    | ClickLink2 ClientId LinkEvent
    | Connect2 ClientId ConnectEvent
    | KeyUp2 ClientId KeyEvent
    | KeyDown2 ClientId KeyEvent
    | PointerDown2 ClientId PointerEvent
    | PointerUp2 ClientId PointerEvent
    | PointerMove2 ClientId PointerEvent
    | PointerLeave2 ClientId PointerEvent
    | PointerCancel2 ClientId PointerEvent
    | PointerOver2 ClientId PointerEvent
    | PointerEnter2 ClientId PointerEvent
    | PointerOut2 ClientId PointerEvent
    | TouchStart2 ClientId TouchEvent
    | TouchCancel2 ClientId TouchEvent
    | TouchMove2 ClientId TouchEvent
    | TouchEnd2 ClientId TouchEvent
    | FromJsPort2 ClientId { port_ : String, data : String }
    | WindowResize2 ClientId WindowResizeEvent
    | SimulateTime Int
    | CheckView2 ClientId CheckViewEvent


eventsToEvent2 : List Event -> List EventType2
eventsToEvent2 events =
    List.foldl
        (\{ clientId, eventType, timestamp } state ->
            case ( eventType, Maybe.map .eventType state.previousEvent ) of
                ( Input input, Just (Input2 _ _) ) ->
                    { previousEvent = Just { eventType = Input2 clientId input, time = timestamp }
                    , rest = state.rest
                    }

                ( Input input, _ ) ->
                    { previousEvent = Just { eventType = Input2 clientId input, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( KeyDown keyDown, _ ) ->
                    { previousEvent = Just { eventType = KeyDown2 clientId keyDown, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( KeyUp keyUp, _ ) ->
                    { previousEvent = Just { eventType = KeyUp2 clientId keyUp, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerDown a, _ ) ->
                    { previousEvent = Just { eventType = PointerDown2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerUp a, _ ) ->
                    { previousEvent = Just { eventType = PointerUp2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerMove a, _ ) ->
                    { previousEvent = Just { eventType = PointerMove2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerLeave a, _ ) ->
                    { previousEvent = Just { eventType = PointerLeave2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerCancel a, _ ) ->
                    { previousEvent = Just { eventType = PointerCancel2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerOver a, _ ) ->
                    { previousEvent = Just { eventType = PointerOver2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerEnter a, _ ) ->
                    { previousEvent = Just { eventType = PointerEnter2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( PointerOut a, _ ) ->
                    { previousEvent = Just { eventType = PointerOut2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( TouchStart a, _ ) ->
                    { previousEvent = Just { eventType = TouchStart2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( TouchCancel a, _ ) ->
                    { previousEvent = Just { eventType = TouchCancel2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( TouchMove a, _ ) ->
                    { previousEvent = Just { eventType = TouchMove2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( TouchEnd a, _ ) ->
                    { previousEvent = Just { eventType = TouchEnd2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( Connect connect, _ ) ->
                    { previousEvent = Just { eventType = Connect2 clientId connect, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( Click mouseEvent, _ ) ->
                    case mouseEvent.targetId of
                        Just targetId ->
                            { previousEvent = Just { eventType = Click2 clientId { targetId = targetId }, time = timestamp }
                            , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                            }

                        Nothing ->
                            state

                ( ClickLink linkEvent, _ ) ->
                    { previousEvent = Just { eventType = ClickLink2 clientId linkEvent, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( Http _, _ ) ->
                    state

                ( HttpLocal _, _ ) ->
                    state

                ( Paste pasteEvent, Just (Input2 previousClientId input) ) ->
                    case pasteEvent.targetId of
                        Just targetId ->
                            if targetId == input.targetId && previousClientId == clientId then
                                { state
                                    | previousEvent = Just { eventType = Input2 clientId { input | text = input.text ++ pasteEvent.text }, time = timestamp }
                                }

                            else
                                { previousEvent = Just { eventType = Input2 clientId { targetId = targetId, text = pasteEvent.text }, time = timestamp }
                                , rest = [ { eventType = Input2 clientId input, time = timestamp } ] ++ state.rest
                                }

                        Nothing ->
                            state

                ( Paste pasteEvent, _ ) ->
                    case pasteEvent.targetId of
                        Just targetId ->
                            { previousEvent = Just { eventType = Input2 clientId { targetId = targetId, text = pasteEvent.text }, time = timestamp }
                            , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                            }

                        Nothing ->
                            state

                ( ResetBackend, _ ) ->
                    state

                ( FromJsPort fromJsPort, _ ) ->
                    case fromJsPort.triggeredFromPort of
                        Just _ ->
                            state

                        Nothing ->
                            { previousEvent =
                                { eventType = FromJsPort2 clientId { port_ = fromJsPort.port_, data = fromJsPort.data }
                                , time = timestamp
                                }
                                    |> Just
                            , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                            }

                ( WindowResize resizeEvent, _ ) ->
                    { previousEvent = Just { eventType = WindowResize2 clientId resizeEvent, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                ( CheckView checkView, _ ) ->
                    { previousEvent = Just { eventType = CheckView2 clientId checkView, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }
        )
        { previousEvent = Nothing, rest = [] }
        events
        |> (\state -> Maybe.Extra.toList state.previousEvent ++ state.rest)
        |> List.reverse
        |> List.foldl
            (\event state ->
                { previousEvent = Just event
                , list =
                    case state.previousEvent of
                        Just { time } ->
                            let
                                delta : Int
                                delta =
                                    event.time - time
                            in
                            if delta > 0 then
                                event.eventType :: SimulateTime delta :: state.list

                            else
                                event.eventType :: state.list

                        Nothing ->
                            event.eventType :: state.list
                }
            )
            { previousEvent = Nothing, list = [] }
        |> .list
        |> List.reverse


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


codegen : { a | includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool } -> List Event -> String
codegen includes events =
    let
        clients : List ClientId
        clients =
            List.map .clientId events |> AssocSet.fromList |> AssocSet.toList

        tests =
            List.Extra.groupWhile (\a _ -> a.eventType /= ResetBackend) events
                |> List.filter (\( _, rest ) -> not (List.isEmpty rest))
                |> List.indexedMap (\index ( head, rest ) -> testCode includes head.timestamp clients index (head :: rest))
                |> String.join "\n    ,"
    in
    setupCode events ++ tests ++ "\n    ]"


testCode : { a | includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool } -> Int -> List ClientId -> Int -> List Event -> String
testCode includes startTime clients testIndex events =
    List.foldl
        (\event { code, indentation, clientCount } ->
            let
                indent =
                    String.repeat indentation "             "

                client clientId =
                    case List.Extra.findIndex (\a -> a == clientId) clients of
                        Just index ->
                            "tab" ++ String.fromInt (index + 1)

                        Nothing ->
                            "tab"
            in
            case event of
                Connect2 clientId { url, sessionId, windowWidth, windowHeight } ->
                    let
                        state =
                            if clientCount == 0 then
                                "state"

                            else
                                "state" ++ String.fromInt clientCount
                    in
                    { code =
                        code
                            ++ (indent ++ "    |> T.connectFrontend\n")
                            ++ (indent ++ "        (Effect.Lamdera.sessionIdFromString \"" ++ sessionId ++ "\")\n")
                            ++ (indent ++ "        (Url.fromString \"" ++ url ++ "\" |> Maybe.withDefault domain)\n")
                            ++ (indent ++ "        { width = " ++ String.fromInt windowWidth ++ ", height = " ++ String.fromInt windowHeight ++ " }\n")
                            ++ (indent ++ "        (\\( " ++ state ++ ", " ++ client clientId ++ ") ->\n")
                            ++ (indent ++ "            " ++ state ++ "\n")
                    , indentation = indentation + 1
                    , clientCount = clientCount + 1
                    }

                WindowResize2 clientId resizeEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".resizeWindow { width = "
                            ++ String.fromInt resizeEvent.width
                            ++ ", height = "
                            ++ String.fromInt resizeEvent.height
                            ++ " }\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyDown2 clientId keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".keyDown "
                            ++ targetIdFunc keyEvent.targetId
                            ++ " \""
                            ++ keyEvent.key
                            ++ "\" [ "
                            ++ String.join ", "
                                (List.filterMap
                                    (\( name, bool ) ->
                                        if bool then
                                            Just name

                                        else
                                            Nothing
                                    )
                                    [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                    , ( "Key_AltHeld", keyEvent.altKey )
                                    , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                    , ( "Key_MetaHeld", keyEvent.metaKey )
                                    ]
                                )
                            ++ " ]\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyUp2 clientId keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".keyUp "
                            ++ targetIdFunc keyEvent.targetId
                            ++ " \""
                            ++ keyEvent.key
                            ++ "\" [ "
                            ++ String.join ", "
                                (List.filterMap
                                    (\( name, bool ) ->
                                        if bool then
                                            Just name

                                        else
                                            Nothing
                                    )
                                    [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                    , ( "Key_AltHeld", keyEvent.altKey )
                                    , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                    , ( "Key_MetaHeld", keyEvent.metaKey )
                                    ]
                                )
                            ++ " ]\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                Click2 clientId mouseEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".clickButton "
                            ++ targetIdFunc mouseEvent.targetId
                            ++ "\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                ClickLink2 clientId mouseEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".clickLink \""
                            ++ mouseEvent.path
                            ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                Input2 clientId { targetId, text } ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".inputText "
                            ++ targetIdFunc targetId
                            ++ " \""
                            ++ text
                            ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                FromJsPort2 _ _ ->
                    { code = code
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerDown2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerDown" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerUp2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerUp" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerMove2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerMove" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerLeave2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerLeave" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerCancel2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerCancel" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerOver2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerOver" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerEnter2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerEnter" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerOut2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerOut" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchStart2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchStart" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchCancel2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchCancel" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchMove2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchMove" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchEnd2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchEnd" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                SimulateTime duration ->
                    { code = code ++ indent ++ "    |> T.simulateTime (Duration.milliseconds " ++ String.fromInt duration ++ ")\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                CheckView2 clientId checkViewEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".checkView (\\single -> Test.Html.Query.has [ "
                            ++ String.join ", " (List.map (\text -> "Selector.text \"" ++ text ++ "\"") checkViewEvent.selection)
                            ++ " ]\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }
        )
        { code =
            " T.start \"test"
                ++ String.fromInt testIndex
                ++ "\" (Time.millisToPosix "
                ++ String.fromInt startTime
                ++ ") config"
                ++ "\n"
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


pointerCodegen : { a | includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool } -> String -> String -> PointerEvent -> String
pointerCodegen { includeClientPos, includePagePos, includeScreenPos } funcName client a =
    let
        options : List String
        options =
            List.filterMap
                (\( name, include, ( x, y ) ) ->
                    if (x == a.offsetX && y == a.offsetY) || not include then
                        Nothing

                    else
                        Just (name ++ " " ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                )
                [ ( "ScreenPos", includeScreenPos, ( a.screenX, a.screenY ) )
                , ( "PagePos", includePagePos, ( a.pageX, a.pageY ) )
                , ( "ClientPos", includeClientPos, ( a.clientX, a.clientY ) )
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
        ++ " ("
        ++ String.fromFloat a.offsetX
        ++ ","
        ++ String.fromFloat a.offsetY
        ++ ") [ "
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

import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Effect.Browser.Dom as Dom
import Effect.Lamdera
import Effect.Test as T exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..), PointerOptions(..))
import Frontend
import Json.Decode
import Json.Encode
import Test.Html.Query
import Test.Html.Selector as Selector
import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
import Url exposing (Url)


setup : T.ViewerWith (List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    T.viewerWith tests
        |> T.addBytesFiles (Dict.values httpRequests)


main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    T.startViewer setup


domain : Url
domain = { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


handlePortToJs : { currentRequest : T.PortToJs, data : T.Data FrontendModel BackendModel } -> Maybe ( String, Json.Decode.Value )
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


{-| Please don't modify or rename this function -}
httpRequests : Dict String String
httpRequests =
    [ """
        ++ httpRequests
        ++ """
    ]
        |> Dict.fromList


handleHttpRequests : Dict String Bytes -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
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


{-| You can change parts of this function represented with `...`.
The rest needs to remain unchanged in order for the test generator to be able to add new tests.

    tests : ... -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
    tests ... =
        let
            config = ...

            ...
        in
        [ ...
        ]
-}
tests : Dict String Bytes -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests httpData =
    let
        config =
            T.Config
                Frontend.app_
                Backend.app_
                (handleHttpRequests httpData)
                handlePortToJs
                (\\_ -> UnhandledFileUpload)
                (\\_ -> UnhandledMultiFileUpload)
                domain
    in
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
                Ui.Anim.layout
                    { options = []
                    , breakpoints = Nothing
                    , toMsg = ElmUiMsg
                    }
                    loaded.elmUiState
                    [ Ui.Font.family [ Ui.Font.sansSerif ], Ui.height Ui.fill ]
                    (loadedView loaded)
        ]
    }


loadedView : LoadedData -> Ui.Element FrontendMsg
loadedView model =
    let
        eventsList : List Event
        eventsList =
            Array.toList model.history
    in
    Ui.row
        [ Ui.height Ui.fill ]
        [ Ui.column
            [ Ui.height Ui.fill
            , Ui.width Ui.shrink
            , Ui.background (Ui.rgb 255 250 245)
            , Ui.borderWith { left = 0, right = 1, top = 0, bottom = 0 }
            , Ui.borderColor (Ui.rgb 245 240 235)
            ]
            [ Ui.row
                []
                [ Ui.el [ Ui.Font.bold, Ui.paddingWith { left = 8, right = 8, top = 8, bottom = 4 } ] (Ui.text "Event history")
                , Ui.el
                    [ Ui.Input.button PressedResetSession
                    , Ui.border 1
                    , Ui.borderColor (Ui.rgb 120 110 100)
                    , Ui.background (Ui.rgb 240 235 230)
                    , Ui.width Ui.shrink
                    , Ui.paddingWith { left = 8, right = 8, top = 10, bottom = 6 }
                    ]
                    (Ui.text "Reset")
                ]
            , eventsView eventsList
            ]
        , Ui.column
            [ Ui.height Ui.fill, Ui.spacing 0 ]
            [ Ui.column
                [ Ui.spacing 8, Ui.padding 8 ]
                [ Ui.Prose.paragraph
                    [ Ui.Font.size 16, Ui.width Ui.shrink, Ui.spacing 4 ]
                    [ Ui.text "Press "
                    , Ui.el
                        [ Ui.Font.color (Ui.rgb 238 238 238)
                        , Ui.background (Ui.rgb 41 51 53)
                        , Ui.Font.size 14
                        , Ui.contentCenterY
                        , Ui.padding 4
                        ]
                        (Ui.text "Reset\u{00A0}Backend")
                    , Ui.text " in lamdera live to start a new test"
                    ]
                , Ui.column
                    []
                    [ simpleCheckbox ToggledIncludeClientPos "Include clientPos in pointer events" model.includeClientPos
                    , simpleCheckbox ToggledIncludePagePos "Include pagePos in pointer events" model.includePagePos
                    , simpleCheckbox ToggledIncludeScreenPos "Include screenPos in pointer events" model.includeScreenPos
                    ]
                ]
            , Ui.el
                [ Ui.borderWith { left = 0, top = 1, bottom = 0, right = 0 }
                , Ui.borderColor (Ui.rgb 100 100 100)
                , Ui.inFront
                    (Ui.row
                        [ Ui.Input.button PressedCopyCode
                        , Ui.border 1
                        , Ui.borderColor (Ui.rgb 100 100 100)
                        , Ui.background (Ui.rgb 240 240 240)
                        , Ui.width Ui.shrink
                        , Ui.padding 4
                        , Ui.roundedWith { topLeft = 0, topRight = 0, bottomRight = 0, bottomLeft = 4 }
                        , Ui.alignRight
                        , Ui.move { x = 0, y = -1, z = 0 }
                        , Ui.Font.size 14
                        , Ui.spacing 4
                        ]
                        [ Icons.copy
                        , if model.copyCounter > 0 then
                            Ui.text "Copied!"

                          else
                            Ui.text "Copy to clipboard"
                        ]
                    )
                ]
                Ui.none
            , List.filter (\event -> not event.isHidden) eventsList
                |> codegen model
                |> Ui.text
                |> Ui.el
                    [ Ui.Font.family [ Ui.Font.monospace ]
                    , Ui.Font.size 12
                    , Ui.Font.exactWhitespace
                    , Ui.scrollable
                    , Ui.padding 8
                    ]
            ]
        ]


simpleCheckbox : (Bool -> msg) -> String -> Bool -> Ui.Element msg
simpleCheckbox msg text value =
    let
        { element, id } =
            Ui.Input.label text [ Ui.Font.size 14, Ui.move { x = 0, y = 2, z = 0 } ] (Ui.text text)
    in
    Ui.row
        [ Ui.spacing 4 ]
        [ Ui.Input.checkbox
            []
            { onChange = msg
            , icon = Nothing
            , checked = value
            , label = id
            }
        , element
        ]


eventsView : List Event -> Ui.Element FrontendMsg
eventsView events =
    case events of
        [] ->
            Ui.el [ Ui.width (Ui.px 240), Ui.padding 16 ] (Ui.text "No events have arrived")

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
                                        "Click " ++ id

                                    Nothing ->
                                        "Click nothing"

                            Http _ ->
                                "Http request"

                            Connect record ->
                                "Connected " ++ record.sessionId

                            ClickLink linkEvent ->
                                "Click link " ++ linkEvent.path

                            Paste pasteEvent ->
                                "Pasted text " ++ ellipsis pasteEvent.text

                            Input inputEvent ->
                                "Text input " ++ ellipsis inputEvent.text

                            ResetBackend ->
                                "Test ended"

                            FromJsPort fromJsPortEvent ->
                                "Port " ++ fromJsPortEvent.port_ ++ " " ++ ellipsis fromJsPortEvent.data

                            HttpLocal { filepath } ->
                                "Loaded " ++ filepath

                            WindowResize { width, height } ->
                                "Window resized w:" ++ String.fromInt width ++ " h:" ++ String.fromInt height

                            PointerDown _ ->
                                "Pointer down"

                            PointerUp _ ->
                                "Pointer up"

                            PointerMove _ ->
                                "Pointer move"

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

                            CheckView _ ->
                                "Check view"
                          )
                            --++ " "
                            --++ String.fromInt event.timestamp
                            |> Ui.text
                        ]
                )
                events
                |> Ui.column
                    [ Ui.width (Ui.px 240)
                    , Ui.Font.size 14
                    , Ui.paddingXY 8 0
                    , Ui.clipWithEllipsis
                    , Ui.paddingWith { left = 0, right = 0, top = 4, bottom = 8 }
                    ]
                |> Ui.el [ Ui.id eventsListContainer, Ui.scrollable, Ui.height Ui.fill, Ui.width Ui.shrink ]


ellipsis : String -> String
ellipsis text =
    if String.length text < 10 then
        text

    else
        String.left 7 text ++ "..."
