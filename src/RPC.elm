module RPC exposing (..)

import AssocList
import AssocSet
import Bytes.Encode
import Json.Decode
import Json.Encode
import Lamdera
import LamderaRPC
import Task
import Types exposing (BackendModel, BackendMsg(..), SessionName, ToFrontend)


lamdera_handleEndpoints :
    Json.Encode.Value
    -> LamderaRPC.HttpRequest
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints _ args model =
    case args.endpoint of
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
