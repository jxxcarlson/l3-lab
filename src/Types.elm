module Types exposing (..)

import Authentication exposing (AuthenticationDict)
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Data exposing (DataDict)
import Document exposing (Document)
import Http
import Markup.API
import Markup.Lang
import Random
import Time
import Url exposing (Url)
import User exposing (User)


type alias FrontendModel =
    { key : Key
    , url : Url
    , message : String

    -- ADMIN
    , users : List User

    -- USER
    , currentUser : Maybe User
    , inputUsername : String
    , inputPassword : String

    -- UI
    , windowWidth : Int
    , windowHeight : Int
    , popupStatus : PopupStatus
    , showEditor : Bool

    -- DOCUMENT
    , currentDocument : Document
    , documents : List Document
    , language : Markup.Lang.Lang
    , inputSearchKey : String
    , printingState : PrintingState
    , documentDeleteState : DocumentDeleteState
    , counter : Int
    }


type PopupWindow
    = AdminPopup


type PopupStatus
    = PopupOpen PopupWindow
    | PopupClosed


type alias BackendModel =
    { message : String
    , currentTime : Time.Posix

    -- RANDOM
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int

    -- DATA
    , dataDict : DataDict

    -- USER
    , authenticationDict : AuthenticationDict

    -- DOCUMENT
    , documents : List Document
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
      -- UI
    | GotNewWindowDimensions Int Int
    | GotViewport Dom.Viewport
    | SetViewPortForElement (Result Dom.Error ( Dom.Element, Dom.Viewport ))
    | ChangePopupStatus PopupStatus
    | ToggleEditor
      -- USER
    | SignIn
    | SignOut
    | InputUsername String
    | InputPassword String
    | GrantGuestAccess
      -- ADMIN
    | AdminRunTask
    | GetUsers
      -- DOC
    | InputText String
    | InputSearchKey String
    | NewDocument
    | SetLanguage Markup.Lang.Lang
    | ChangeDocumentDeleteStateFrom DocumentDeleteState
    | AskFoDocumentById String
    | AskFoDocumentBySlug String
    | FetchDocuments SearchTerm
    | ExportToMarkdown
    | ExportToLaTeX
    | Export
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | ChangePrintingState PrintingState
    | FinallyDoCleanPrintArtefacts String
    | ToggleAccess
    | Help String


type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady


type DocumentDeleteState
    = WaitingForDeleteAction
    | DocumentDeletePending


type SearchTerm
    = Query String


type ToBackend
    = NoOpToBackend
      -- ADMIN
    | RunTask
    | SendUsers
      -- USER
    | SignInOrSignUp String String
      -- DOCUMENT
    | SaveDocument Document
    | DeleteDocumentById String
    | GetUserDocuments String
    | GetDocumentsWithQuery (Maybe User) SearchTerm
    | GetDocumentById String
    | GetDocumentBySlug String
    | GetDocumentBySlugForGuest String
    | RegisterNewDocument Document


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)
    | Tick Time.Posix


type ToFrontend
    = NoOpToFrontend
      -- ADMIN
    | GotUsers (List User)
      -- USER
    | SendUser User
    | LoginGuest
      -- DOCUMENT
    | SendDocument Document
    | SendDocuments (List Document)
    | SendMessage String
