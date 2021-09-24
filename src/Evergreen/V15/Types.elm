module Evergreen.V15.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V15.Authentication
import Evergreen.V15.Common.Syntax
import Evergreen.V15.Data
import Evergreen.V15.Document
import Evergreen.V15.User
import Http
import Random
import Time
import Url


type PopupWindow
    = AdminPopup


type PopupStatus
    = PopupOpen PopupWindow
    | PopupClosed


type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady


type DocumentDeleteState
    = WaitingForDeleteAction
    | DocumentDeletePending


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , message : String
    , users : List Evergreen.V15.User.User
    , currentUser : Maybe Evergreen.V15.User.User
    , inputUsername : String
    , inputPassword : String
    , windowWidth : Int
    , windowHeight : Int
    , popupStatus : PopupStatus
    , showEditor : Bool
    , currentDocument : Evergreen.V15.Document.Document
    , documents : List Evergreen.V15.Document.Document
    , language : Evergreen.V15.Common.Syntax.Language
    , inputSearchKey : String
    , printingState : PrintingState
    , documentDeleteState : DocumentDeleteState
    , counter : Int
    }


type alias BackendModel =
    { message : String
    , currentTime : Time.Posix
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int
    , dataDict : Evergreen.V15.Data.DataDict
    , authenticationDict : Evergreen.V15.Authentication.AuthenticationDict
    , documents : List Evergreen.V15.Document.Document
    }


type SearchTerm
    = Query String


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GotNewWindowDimensions Int Int
    | GotViewport Browser.Dom.Viewport
    | SetViewPortForElement (Result Browser.Dom.Error ( Browser.Dom.Element, Browser.Dom.Viewport ))
    | ChangePopupStatus PopupStatus
    | ToggleEditor
    | SignIn
    | SignOut
    | InputUsername String
    | InputPassword String
    | GrantGuestAccess
    | AdminRunTask
    | GetUsers
    | InputText String
    | InputSearchKey String
    | NewDocument
    | SetLanguage Evergreen.V15.Common.Syntax.Language
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


type ToBackend
    = NoOpToBackend
    | RunTask
    | SendUsers
    | SignInOrSignUp String String
    | SaveDocument Evergreen.V15.Document.Document
    | DeleteDocumentById String
    | GetUserDocuments String
    | GetDocumentsWithQuery (Maybe Evergreen.V15.User.User) SearchTerm
    | GetDocumentById String
    | GetDocumentBySlug String
    | GetDocumentBySlugForGuest String
    | RegisterNewDocument Evergreen.V15.Document.Document


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)
    | Tick Time.Posix


type ToFrontend
    = NoOpToFrontend
    | GotUsers (List Evergreen.V15.User.User)
    | SendUser Evergreen.V15.User.User
    | LoginGuest
    | SendDocument Evergreen.V15.Document.Document
    | SendDocuments (List Evergreen.V15.Document.Document)
    | SendMessage String
