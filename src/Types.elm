module Types exposing (..)

import Array exposing (Array)
import AssocList
import AssocSet
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import SessionName exposing (SessionName)
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
    , copyCounter : Int
    , elmUiState : Ui.Anim.State
    , settings : Settings
    , parsedCode : ParsedCodeStatus
    , mouseDownOnEvent : Bool
    , commitStatus : CommitStatus
    , noEventsHaveArrived : Bool
    }


type CommitStatus
    = NotCommitted
    | Committing String
    | CommitSuccess
    | CommitFailed


type alias Settings =
    { includeClientPos : Bool
    , includePagePos : Bool
    , includeScreenPos : Bool
    , showAllCode : Bool
    }


type ParsedCodeStatus
    = ParseSuccess ParsedCode
    | ParseFailed ParseError
    | WaitingOnUser
    | FileApiNotSupported


type ParseError
    = InvalidPortRequests
    | InvalidHttpRequests
    | InvalidHttpAndPortRequests
    | PortRequestsNotFound
    | HttpRequestsNotFound
    | TestEntryPointNotFound
    | PortRequestsEndNotFound
    | HttpRequestsEndNotFound
    | UnknownError


type alias ParsedCode =
    { codeParts : List Code
    , httpRequests : List ( String, String )
    , portRequests : List ( String, ( String, String ) )
    , noPriorTests : Bool
    }


type Code
    = UserCode String
    | HttpRequestCode
    | PortRequestCode
    | TestEntryPoint


type alias BackendModel =
    { sessions : AssocList.Dict SessionName Session
    }


type alias Session =
    { history : Array Event
    , connections : AssocSet.Set ClientId
    , settings : Settings
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedResetSession
    | GotRandomSessionName SessionName
    | ScrolledToBottom
    | MouseDownOnEvent Int Bool
    | MouseEnterOnEvent Int Bool
    | ElmUiMsg Ui.Anim.Msg
    | ToggledIncludeScreenPos Bool
    | ToggledIncludeClientPos Bool
    | ToggledIncludePagePos Bool
    | MouseUpEvent
    | PressedEvent
    | GotFile { name : String, content : String }
    | PressedSelectFile
    | PressedNewFile
    | PressedCommitToFile
    | FileApiNotSupportedFromPort
    | ToggledShowAllCode Bool
    | WroteToFile Bool
    | PressedDownloadTestGen


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest
    | SetEventVisibilityRequest { index : Int, isHidden : Bool }
    | SetSettingsRequest Settings


type BackendMsg
    = ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse { history : Array Event, settings : Settings }
    | SessionUpdate Event
    | ResetSession


type alias Event =
    { isHidden : Bool
    , timestamp : Int
    , eventType : EventType
    , clientId : ClientId
    }


type EventType
    = KeyDown KeyEvent
    | KeyUp KeyEvent
    | Click ClickEvent
    | ClickLink LinkEvent
    | Http HttpEvent
    | HttpLocal HttpLocalEvent
    | Connect ConnectEvent
    | Paste PasteEvent
    | Input InputEvent
    | ResetBackend
    | FromJsPort FromJsPortEvent
    | WindowResize WindowResizeEvent
    | PointerDown PointerEvent
    | PointerUp PointerEvent
    | PointerMove PointerEvent
    | PointerLeave PointerEvent
    | PointerCancel PointerEvent
    | PointerOver PointerEvent
    | PointerEnter PointerEvent
    | PointerOut PointerEvent
    | TouchStart TouchEvent
    | TouchCancel TouchEvent
    | TouchMove TouchEvent
    | TouchEnd TouchEvent
    | CheckView CheckViewEvent
    | MouseDown MouseEvent
    | MouseUp MouseEvent
    | MouseMove MouseEvent
    | MouseLeave MouseEvent
    | MouseOver MouseEvent
    | MouseEnter MouseEvent
    | MouseOut MouseEvent
    | Focus FocusEvent
    | Blur BlurEvent
    | Wheel WheelEvent


type alias FocusEvent =
    { targetId : String }


type alias BlurEvent =
    { targetId : String }


type alias CheckViewEvent =
    { selection : List String
    }


type alias PointerEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    , pointerType : String
    , pointerId : Int
    , isPrimary : Bool
    , width : Float
    , height : Float
    , pressure : Float
    , tiltX : Float
    , tiltY : Float
    }


type alias MouseEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    }


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    , deltaZ : Float
    , deltaMode : Int
    , mouseEvent : MouseEvent
    }


type alias TouchEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , changedTouches : List Touch
    , targetTouches : List Touch
    , touches : List Touch
    }


type alias Touch =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , identifier : Int
    }


type alias ConnectEvent =
    { url : String, sessionId : SessionId, windowWidth : Int, windowHeight : Int }


type alias WindowResizeEvent =
    { width : Int, height : Int }


type alias HttpLocalEvent =
    { filepath : String }


type alias FromJsPortEvent =
    { triggeredFromPort : Maybe String, port_ : String, data : String }


type alias InputEvent =
    { targetId : String, text : String }


type alias KeyEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , key : String
    }


type alias ClickEvent =
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
