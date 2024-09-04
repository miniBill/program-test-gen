module RPC exposing (..)

import Array
import AssocList
import AssocSet
import Codec exposing (Codec)
import Frontend
import Json.Decode
import Json.Encode
import Lamdera
import LamderaRPC exposing (HttpBody(..))
import SessionName exposing (SessionName)
import Types exposing (BackendModel, BackendMsg(..), Event, ToFrontend(..))


type alias EventEndpoint =
    { event : Event
    , sessionName : SessionName
    }


eventEndpointCodec : Codec EventEndpoint
eventEndpointCodec =
    Codec.object EventEndpoint
        |> Codec.field "event" .event eventCodec
        |> Codec.field "sessionName" .sessionName SessionName.codec
        |> Codec.buildObject


eventCodec : Codec Event
eventCodec =
    Codec.object (Event False)
        |> Codec.field "timestamp" .timestamp Codec.int
        |> Codec.field "eventType" .eventType eventTypeCodec
        |> Codec.field "clientId" .clientId Codec.string
        |> Codec.buildObject


eventTypeCodec : Codec Types.EventType
eventTypeCodec =
    Codec.custom
        (\keyDownEncoder keyUpEncoder clickEncoder clickLinkEncoder httpEncoder httpLocalEncoder connectEncoder pasteEncoder inputEncoder resetBackendEncoder fromJsPortEncoder windowResizeEncoder pointerDownEncoder pointerUpEncoder pointerMoveEncoder pointerLeaveEncoder pointerCancelEncoder pointerOverEncoder pointerEnterEncoder pointerOutEncoder touchStartEncoder touchCancelEncoder touchMoveEncoder touchEndEncoder checkViewEncoder mouseDownEncoder mouseUpEncoder mouseMoveEncoder mouseLeaveEncoder mouseOverEncoder mouseEnterEncoder mouseOutEncoder focusEncoder blurEncoder value ->
            case value of
                Types.KeyDown arg0 ->
                    keyDownEncoder arg0

                Types.KeyUp arg0 ->
                    keyUpEncoder arg0

                Types.Click arg0 ->
                    clickEncoder arg0

                Types.ClickLink arg0 ->
                    clickLinkEncoder arg0

                Types.Http arg0 ->
                    httpEncoder arg0

                Types.HttpLocal arg0 ->
                    httpLocalEncoder arg0

                Types.Connect arg0 ->
                    connectEncoder arg0

                Types.Paste arg0 ->
                    pasteEncoder arg0

                Types.Input arg0 ->
                    inputEncoder arg0

                Types.ResetBackend ->
                    resetBackendEncoder

                Types.FromJsPort arg0 ->
                    fromJsPortEncoder arg0

                Types.WindowResize arg0 ->
                    windowResizeEncoder arg0

                Types.PointerDown arg0 ->
                    pointerDownEncoder arg0

                Types.PointerUp arg0 ->
                    pointerUpEncoder arg0

                Types.PointerMove arg0 ->
                    pointerMoveEncoder arg0

                Types.PointerLeave arg0 ->
                    pointerLeaveEncoder arg0

                Types.PointerCancel arg0 ->
                    pointerCancelEncoder arg0

                Types.PointerOver arg0 ->
                    pointerOverEncoder arg0

                Types.PointerEnter arg0 ->
                    pointerEnterEncoder arg0

                Types.PointerOut arg0 ->
                    pointerOutEncoder arg0

                Types.TouchStart arg0 ->
                    touchStartEncoder arg0

                Types.TouchCancel arg0 ->
                    touchCancelEncoder arg0

                Types.TouchMove arg0 ->
                    touchMoveEncoder arg0

                Types.TouchEnd arg0 ->
                    touchEndEncoder arg0

                Types.CheckView arg0 ->
                    checkViewEncoder arg0

                Types.MouseDown arg0 ->
                    mouseDownEncoder arg0

                Types.MouseUp arg0 ->
                    mouseUpEncoder arg0

                Types.MouseMove arg0 ->
                    mouseMoveEncoder arg0

                Types.MouseLeave arg0 ->
                    mouseLeaveEncoder arg0

                Types.MouseOver arg0 ->
                    mouseOverEncoder arg0

                Types.MouseEnter arg0 ->
                    mouseEnterEncoder arg0

                Types.MouseOut arg0 ->
                    mouseOutEncoder arg0

                Types.Focus arg0 ->
                    focusEncoder arg0

                Types.Blur arg0 ->
                    blurEncoder arg0
        )
        |> Codec.variant1 "KeyDown" Types.KeyDown keyEventCodec
        |> Codec.variant1 "KeyUp" Types.KeyUp keyEventCodec
        |> Codec.variant1 "Click" Types.Click clickEventCodec
        |> Codec.variant1 "ClickLink" Types.ClickLink linkEventCodec
        |> Codec.variant1 "Http" Types.Http httpEventCodec
        |> Codec.variant1 "HttpLocal" Types.HttpLocal httpLocalEventCodec
        |> Codec.variant1 "Connect" Types.Connect connectEventCodec
        |> Codec.variant1 "Paste" Types.Paste pasteEventCodec
        |> Codec.variant1 "Input" Types.Input inputEventCodec
        |> Codec.variant0 "ResetBackend" Types.ResetBackend
        |> Codec.variant1 "FromJsPort" Types.FromJsPort fromJsPortEventCodec
        |> Codec.variant1 "WindowResize" Types.WindowResize windowResizeEventCodec
        |> Codec.variant1 "PointerDown" Types.PointerDown pointerEventCodec
        |> Codec.variant1 "PointerUp" Types.PointerUp pointerEventCodec
        |> Codec.variant1 "PointerMove" Types.PointerMove pointerEventCodec
        |> Codec.variant1 "PointerLeave" Types.PointerLeave pointerEventCodec
        |> Codec.variant1 "PointerCancel" Types.PointerCancel pointerEventCodec
        |> Codec.variant1 "PointerOver" Types.PointerOver pointerEventCodec
        |> Codec.variant1 "PointerEnter" Types.PointerEnter pointerEventCodec
        |> Codec.variant1 "PointerOut" Types.PointerOut pointerEventCodec
        |> Codec.variant1 "TouchStart" Types.TouchStart touchEventCodec
        |> Codec.variant1 "TouchCancel" Types.TouchCancel touchEventCodec
        |> Codec.variant1 "TouchMove" Types.TouchMove touchEventCodec
        |> Codec.variant1 "TouchEnd" Types.TouchEnd touchEventCodec
        |> Codec.variant1 "CheckView" Types.CheckView checkViewCodec
        |> Codec.variant1 "MouseDown" Types.MouseDown mouseEventCodec
        |> Codec.variant1 "MouseUp" Types.MouseUp mouseEventCodec
        |> Codec.variant1 "MouseMove" Types.MouseMove mouseEventCodec
        |> Codec.variant1 "MouseLeave" Types.MouseLeave mouseEventCodec
        |> Codec.variant1 "MouseOver" Types.MouseOver mouseEventCodec
        |> Codec.variant1 "MouseEnter" Types.MouseEnter mouseEventCodec
        |> Codec.variant1 "MouseOut" Types.MouseOut mouseEventCodec
        |> Codec.variant1 "Focus" Types.Focus focusEventCodec
        |> Codec.variant1 "Blur" Types.Blur blurEventCodec
        |> Codec.buildCustom


focusEventCodec : Codec Types.FocusEvent
focusEventCodec =
    Codec.object Types.FocusEvent |> Codec.field "targetId" .targetId Codec.string |> Codec.buildObject


blurEventCodec : Codec Types.BlurEvent
blurEventCodec =
    Codec.object Types.BlurEvent |> Codec.field "targetId" .targetId Codec.string |> Codec.buildObject


mouseEventCodec : Codec Types.MouseEvent
mouseEventCodec =
    Codec.object Types.MouseEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "altKey" .altKey Codec.bool
        |> Codec.field "clientX" .clientX Codec.float
        |> Codec.field "clientY" .clientY Codec.float
        |> Codec.field "offsetX" .offsetX Codec.float
        |> Codec.field "offsetY" .offsetY Codec.float
        |> Codec.field "pageX" .pageX Codec.float
        |> Codec.field "pageY" .pageY Codec.float
        |> Codec.field "screenX" .screenX Codec.float
        |> Codec.field "screenY" .screenY Codec.float
        |> Codec.field "button" .button Codec.int
        |> Codec.buildObject


checkViewCodec : Codec Types.CheckViewEvent
checkViewCodec =
    Codec.object Types.CheckViewEvent
        |> Codec.field "selection" .selection (Codec.list Codec.string)
        |> Codec.buildObject


pointerEventCodec : Codec Types.PointerEvent
pointerEventCodec =
    Codec.object Types.PointerEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "altKey" .altKey Codec.bool
        |> Codec.field "clientX" .clientX Codec.float
        |> Codec.field "clientY" .clientY Codec.float
        |> Codec.field "offsetX" .offsetX Codec.float
        |> Codec.field "offsetY" .offsetY Codec.float
        |> Codec.field "pageX" .pageX Codec.float
        |> Codec.field "pageY" .pageY Codec.float
        |> Codec.field "screenX" .screenX Codec.float
        |> Codec.field "screenY" .screenY Codec.float
        |> Codec.field "button" .button Codec.int
        |> Codec.field "pointerType" .pointerType Codec.string
        |> Codec.field "pointerId" .pointerId Codec.int
        |> Codec.field "isPrimary" .isPrimary Codec.bool
        |> Codec.field "width" .width Codec.float
        |> Codec.field "height" .height Codec.float
        |> Codec.field "pressure" .pressure Codec.float
        |> Codec.field "tiltX" .tiltX Codec.float
        |> Codec.field "tiltY" .tiltY Codec.float
        |> Codec.buildObject


touchEventCodec : Codec Types.TouchEvent
touchEventCodec =
    Codec.object Types.TouchEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "altKey" .altKey Codec.bool
        |> Codec.field "changedTouches" .changedTouches (Codec.list touchCodec)
        |> Codec.field "targetTouches" .targetTouches (Codec.list touchCodec)
        |> Codec.field "touches" .touches (Codec.list touchCodec)
        |> Codec.buildObject


touchCodec : Codec Types.Touch
touchCodec =
    Codec.object Types.Touch
        |> Codec.field "clientX" .clientX Codec.float
        |> Codec.field "clientY" .clientY Codec.float
        |> Codec.field "pageX" .pageX Codec.float
        |> Codec.field "pageY" .pageY Codec.float
        |> Codec.field "screenX" .screenX Codec.float
        |> Codec.field "screenY" .screenY Codec.float
        |> Codec.field "identifier" .identifier Codec.int
        |> Codec.buildObject


connectEventCodec : Codec Types.ConnectEvent
connectEventCodec =
    Codec.object Types.ConnectEvent
        |> Codec.field "url" .url Codec.string
        |> Codec.field "sessionId" .sessionId Codec.string
        |> Codec.field "windowWidth" .windowWidth Codec.int
        |> Codec.field "windowHeight" .windowHeight Codec.int
        |> Codec.buildObject


keyEventCodec : Codec Types.KeyEvent
keyEventCodec =
    Codec.object Types.KeyEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "altKey" .metaKey Codec.bool
        |> Codec.field "key" .key Codec.string
        |> Codec.buildObject


clickEventCodec : Codec Types.ClickEvent
clickEventCodec =
    Codec.object Types.ClickEvent
        |> Codec.field "targetId" .targetId (Codec.nullable Codec.string)
        |> Codec.buildObject


pasteEventCodec : Codec Types.PasteEvent
pasteEventCodec =
    Codec.object Types.PasteEvent
        |> Codec.field "targetId" .targetId (Codec.nullable Codec.string)
        |> Codec.field "text" .text Codec.string
        |> Codec.buildObject


inputEventCodec : Codec Types.InputEvent
inputEventCodec =
    Codec.object Types.InputEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "text" .text Codec.string
        |> Codec.buildObject


fromJsPortEventCodec : Codec Types.FromJsPortEvent
fromJsPortEventCodec =
    Codec.object Types.FromJsPortEvent
        |> Codec.nullableField "triggeredFromPort" .triggeredFromPort Codec.string
        |> Codec.field "port" .port_ Codec.string
        |> Codec.field "data" .data Codec.string
        |> Codec.buildObject


httpLocalEventCodec : Codec Types.HttpLocalEvent
httpLocalEventCodec =
    Codec.object Types.HttpLocalEvent
        |> Codec.field "filepath" .filepath Codec.string
        |> Codec.buildObject


windowResizeEventCodec : Codec Types.WindowResizeEvent
windowResizeEventCodec =
    Codec.object Types.WindowResizeEvent
        |> Codec.field "width" .width Codec.int
        |> Codec.field "height" .height Codec.int
        |> Codec.buildObject


linkEventCodec : Codec Types.LinkEvent
linkEventCodec =
    Codec.object Types.LinkEvent
        |> Codec.field "path" .path Codec.string
        |> Codec.buildObject


httpEventCodec : Codec Types.HttpEvent
httpEventCodec =
    Codec.object Types.HttpEvent
        |> Codec.field "responseType" .responseType Codec.string
        |> Codec.field "method" .method Codec.string
        |> Codec.field "url" .url Codec.string
        |> Codec.field "filepath" .filepath Codec.string
        |> Codec.buildObject


lamdera_handleEndpoints :
    Json.Encode.Value
    -> LamderaRPC.HttpRequest
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints _ args model =
    case args.endpoint of
        "event" ->
            case args.body of
                BodyString json ->
                    case Codec.decodeString eventEndpointCodec json of
                        Ok { sessionName, event } ->
                            let
                                model2 : BackendModel
                                model2 =
                                    { model
                                        | sessions =
                                            AssocList.update
                                                sessionName
                                                (\maybeSession ->
                                                    (case maybeSession of
                                                        Just session ->
                                                            Frontend.addEvent event session

                                                        Nothing ->
                                                            { history = Array.fromList [ event ]
                                                            , connections = AssocSet.empty
                                                            , settings =
                                                                { includeClientPos = True
                                                                , includePagePos = True
                                                                , includeScreenPos = False
                                                                , showAllCode = True
                                                                }
                                                            }
                                                    )
                                                        |> Just
                                                )
                                                model.sessions
                                    }
                            in
                            ( LamderaRPC.ResultString ""
                            , model2
                            , broadcastToClients sessionName (SessionUpdate event) model2
                            )

                        Err error ->
                            ( Json.Decode.errorToString error |> LamderaRPC.ResultString
                            , model
                            , Cmd.none
                            )

                _ ->
                    ( LamderaRPC.ResultString ""
                    , model
                    , Cmd.none
                    )

        _ ->
            ( LamderaRPC.ResultString ""
            , model
            , Cmd.none
            )


broadcastToClients : SessionName -> ToFrontend -> BackendModel -> Cmd BackendMsg
broadcastToClients sessionName toFrontend model =
    case AssocList.get sessionName model.sessions of
        Just session ->
            AssocSet.toList session.connections
                |> List.map (\clientId -> Lamdera.sendToFrontend clientId toFrontend)
                |> Cmd.batch

        Nothing ->
            Cmd.none
