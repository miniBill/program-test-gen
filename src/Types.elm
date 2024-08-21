module Types exposing (..)

import Array exposing (Array)
import AssocList
import AssocSet
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Ui.Anim
import Url exposing (Url)


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias LoadingData =
    { key : Key, sessionName : SessionName }


type alias LoadedData =
    { key : Key
    , sessionName : SessionName
    , history : Array Event
    , hiddenEvents : Set Int
    , copyCounter : Int
    , elmUiState : Ui.Anim.State
    }


type alias BackendModel =
    { sessions : AssocList.Dict SessionName Session
    }


type alias Session =
    { history : Array Event
    , connections : AssocSet.Set ClientId
    , hiddenEvents : Set Int
    }


type SessionName
    = SessionName String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedResetSession
    | GotRandomSessionName SessionName
    | ScrolledToBottom
    | PressedSetEventVisibility Int Bool
    | PressedCopyCode
    | ElmUiMsg Ui.Anim.Msg


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest
    | SetEventVisibilityRequest { index : Int, isHidden : Bool }


type BackendMsg
    = ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse (Array Event) (Set Int)
    | SessionUpdate Event
    | ResetSession


type alias Event =
    { timestamp : Int
    , eventType : EventType
    , clientId : ClientId
    }


type EventType
    = KeyDown KeyEvent
    | KeyUp KeyEvent
    | Click MouseEvent
    | ClickLink LinkEvent
    | Http HttpEvent
    | Connect { url : String, sessionId : SessionId, windowWidth : Int, windowHeight : Int }
    | Paste PasteEvent
    | Input InputEvent


type alias InputEvent =
    { targetId : String, text : String }


type alias KeyEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , key : String
    , keyCode : Int
    }


type alias MouseEvent =
    { targetId : Maybe String
    }


type alias PasteEvent =
    { targetId : Maybe String
    , text : String
    }


type alias LinkEvent =
    { path : String
    }


type alias HttpEvent =
    { responseType : String
    , method : String
    , url : String
    , filepath : String
    }
