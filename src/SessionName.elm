module SessionName exposing (SessionName(..), codec, fromString, toString)

import Codec exposing (Codec)


type SessionName
    = SessionName String


toString : SessionName -> String
toString (SessionName a) =
    a


fromString : String -> SessionName
fromString =
    SessionName


codec : Codec SessionName
codec =
    Codec.map fromString toString Codec.string
