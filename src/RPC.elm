module RPC exposing (..)

import Array
import AssocList
import AssocSet
import Codec exposing (Codec)
import Dict
import Frontend
import Json.Decode
import Json.Encode
import Lamdera
import LamderaRPC exposing (HttpBody(..))
import Set
import Types exposing (BackendModel, BackendMsg(..), Event, SessionName(..), ToFrontend(..))


type alias EventEndpoint =
    { event : Event
    , sessionName : SessionName
    }


eventEndpointCodec : Codec EventEndpoint
eventEndpointCodec =
    Codec.object EventEndpoint
        |> Codec.field "event" .event eventCodec
        |> Codec.field "sessionName" .sessionName sessionNameCodec
        |> Codec.buildObject


sessionNameCodec : Codec SessionName
sessionNameCodec =
    Codec.map SessionName (\(SessionName a) -> a) Codec.string


eventCodec : Codec Event
eventCodec =
    Codec.object Event
        |> Codec.field "timestamp" .timestamp Codec.int
        |> Codec.field "eventType" .eventType eventTypeCodec
        |> Codec.field "clientId" .clientId Codec.string
        |> Codec.buildObject


eventTypeCodec : Codec Types.EventType
eventTypeCodec =
    Codec.custom
        (\keyDownEncoder keyUpEncoder clickEncoder linkEncoder httpEncoder connectEncoder pasteEncoder inputEncoder value ->
            case value of
                Types.KeyDown arg0 ->
                    keyDownEncoder arg0

                Types.KeyUp arg0 ->
                    keyUpEncoder arg0

                Types.Click arg0 ->
                    clickEncoder arg0

                Types.ClickLink arg0 ->
                    linkEncoder arg0

                Types.Http arg0 ->
                    httpEncoder arg0

                Types.Connect arg0 ->
                    connectEncoder arg0

                Types.Paste arg0 ->
                    pasteEncoder arg0

                Types.Input arg0 ->
                    inputEncoder arg0
        )
        |> Codec.variant1 "KeyDown" Types.KeyDown keyEventCodec
        |> Codec.variant1 "KeyUp" Types.KeyUp keyEventCodec
        |> Codec.variant1 "Click" Types.Click mouseEventCodec
        |> Codec.variant1 "ClickLink" Types.ClickLink linkEventCodec
        |> Codec.variant1 "Http" Types.Http httpEventCodec
        |> Codec.variant1
            "Connect"
            Types.Connect
            (Codec.object
                (\url sessionId windowWidth windowHeight ->
                    { url = url, sessionId = sessionId, windowWidth = windowWidth, windowHeight = windowHeight }
                )
                |> Codec.field "url" .url Codec.string
                |> Codec.field "sessionId" .sessionId Codec.string
                |> Codec.field "windowWidth" .windowWidth Codec.int
                |> Codec.field "windowHeight" .windowHeight Codec.int
                |> Codec.buildObject
            )
        |> Codec.variant1 "Paste" Types.Paste pasteEventCodec
        |> Codec.variant1 "Input" Types.Input inputEventCodec
        |> Codec.buildCustom


keyEventCodec : Codec Types.KeyEvent
keyEventCodec =
    Codec.object Types.KeyEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "key" .key Codec.string
        |> Codec.field "keyCode" .keyCode Codec.int
        |> Codec.buildObject


mouseEventCodec : Codec Types.MouseEvent
mouseEventCodec =
    Codec.object Types.MouseEvent
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
    let
        _ =
            Debug.log "endpoint" args
    in
    case args.endpoint of
        "event" ->
            case args.body of
                BodyString json ->
                    case Codec.decodeString eventEndpointCodec json |> Debug.log "result" of
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
                                                            { session | history = Frontend.addEvent event session.history }

                                                        Nothing ->
                                                            { history = Array.fromList [ event ]
                                                            , connections = AssocSet.empty
                                                            , hiddenEvents = Set.empty
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
