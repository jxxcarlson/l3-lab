module Evergreen.V1.Document exposing (..)

import Evergreen.V1.Common.Syntax
import Time


type alias Username =
    String


type Access
    = Public
    | Private
    | Shared
        { canRead : List Username
        , canWrite : List Username
        }


type alias Document =
    { title : String
    , author : String
    , username : String
    , id : String
    , created : Time.Posix
    , modified : Time.Posix
    , tags : List String
    , content : String
    , access : Access
    , slug : Maybe String
    , language : Evergreen.V1.Common.Syntax.Language
    }
