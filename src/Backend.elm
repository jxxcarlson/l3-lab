module Backend exposing (..)

import Authentication
import Backend.Cmd
import Backend.Update
import Data
import Dict
import Document exposing (Access(..))
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import List.Extra
import Random
import Time
import Token
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Time.every 10000 Tick
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"

      -- RANDOM
      , randomSeed = Random.initialSeed 1234
      , uuidCount = 0
      , randomAtmosphericInt = Nothing
      , currentTime = Time.millisToPosix 0

      -- DATA
      , dataDict = Dict.empty

      -- USER
      , authenticationDict = Dict.empty

      -- DOCUMENTS
      , documents =
            [ Data.aboutCayatex
            , Data.docsNotFound
            , Data.notSignedIn
            , Data.foo
            ]
      }
    , Backend.Cmd.getRandomNumber
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotAtomsphericRandomNumber result ->
            Backend.Update.gotAtomsphericRandomNumber model result

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        -- ADMIN
        RunTask ->
            let
                documents =
                    List.map (\doc -> { doc | slug = Just (Document.makeSlug model.currentTime doc) }) model.documents
            in
            ( { model | documents = documents }, sendToFrontend clientId (SendMessage <| "doc ids remapped") )

        SendUsers ->
            ( model, sendToFrontend clientId (GotUsers (Authentication.users model.authenticationDict)) )

        -- USER
        SignInOrSignUp username encryptedPassword ->
            let
                _ =
                    Debug.log "XXX (1), SignInOrSignUp for" username
            in
            case Dict.get username model.authenticationDict of
                Just userData ->
                    let
                        _ =
                            Debug.log "XXX (2), SignInOrSignUp for" username
                    in
                    if Authentication.verify username encryptedPassword model.authenticationDict then
                        let
                            predicate =
                                if username == "guest" then
                                    \doc -> doc.access == Public

                                else
                                    \doc -> doc.username == username
                        in
                        ( model
                        , Cmd.batch
                            [ sendToFrontend clientId (SendDocuments (List.filter predicate model.documents))
                            , sendToFrontend clientId (SendUser userData.user)
                            ]
                        )

                    else
                        ( model, sendToFrontend clientId (SendMessage <| "Sorry, password and username don't match") )

                Nothing ->
                    Backend.Update.setupUser model clientId username encryptedPassword

        -- DOCUMENTS
        GetUserDocuments username ->
            let
                _ =
                    Debug.log "XXX, GetUserDocuments for" username
            in
            case username of
                "guest" ->
                    ( model, sendToFrontend clientId (SendDocuments (List.filter (\doc -> doc.access == Public) model.documents)) )

                _ ->
                    ( model, sendToFrontend clientId (SendDocuments (List.filter (\doc -> doc.username == username) model.documents)) )

        GetDocumentsWithQuery user (Query searchTerm) ->
            let
                _ =
                    Debug.log "XXX,  GetDocumentsWithQuery" user

                username =
                    Maybe.map .username user |> Debug.log "XXX, USERNAME"

                docsFound =
                    Document.search user searchTerm model.documents
                        |> List.filter (\doc -> Just doc.username == username || doc.access == Public)
            in
            ( model, sendToFrontend clientId (SendDocuments docsFound) )

        RegisterNewDocument doc_ ->
            let
                { token, seed } =
                    Token.get model.randomSeed

                doc =
                    { doc_
                        | id = token
                        , created = model.currentTime
                        , modified = model.currentTime
                        , slug = Just (Document.makeSlug model.currentTime doc_)
                    }

                newDocuments =
                    doc :: model.documents

                message =
                    "Registered document: " ++ doc.title ++ "(" ++ doc.username ++ ")"
            in
            ( { model | randomSeed = seed, documents = newDocuments }
            , Cmd.batch
                [ sendToFrontend clientId (SendDocument doc)
                , sendToFrontend clientId (SendMessage message)
                ]
            )

        SaveDocument document ->
            let
                newDocuments =
                    List.Extra.setIf (\doc -> doc.id == document.id) document model.documents
            in
            ( { model | documents = newDocuments }, sendToFrontend clientId (SendMessage ("Saved document: " ++ document.title)) )

        DeleteDocumentById id ->
            ( { model | documents = List.filter (\doc -> doc.id /= id) model.documents }, Cmd.none )

        GetDocumentById id ->
            case List.head (List.filter (\doc -> doc.id == id) model.documents) of
                Nothing ->
                    ( model
                    , sendToFrontend clientId (SendMessage <| "Could not find document: " ++ id)
                    )

                Just doc ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend clientId (SendDocument doc)
                        ]
                    )

        GetDocumentBySlug slug ->
            case List.head (List.filter (\doc -> doc.slug == Just slug) model.documents) of
                Nothing ->
                    ( model
                    , sendToFrontend clientId (SendMessage <| "Could not find document by slug (1): " ++ slug)
                    )

                Just doc ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend clientId (SendDocument doc)
                        ]
                    )

        GetDocumentBySlugForGuest path_ ->
            let
                path =
                    if path_ == "/" then
                        "/s/jxxcarlson-welcome-to-l1-2021-07-29"

                    else if path_ == "/guest" then
                        "/g/jxxcarlson-welcome-to-l1-2021-07-29"

                    else
                        path_
            in
            if String.left 3 path == "/g/" then
                signInAsGuestWithDoc model clientId path

            else if String.left 3 path == "/s/" then
                sendDoc model clientId path

            else
                sendDoc model clientId "https://l1-lab.lamdera.app/s/jxxcarlson-welcome-to-l1-2021-07-29"


sendDoc model clientId path =
    let
        slug =
            String.dropLeft 3 path
    in
    case List.head (List.filter (\doc -> doc.slug == Just slug) model.documents) of
        Nothing ->
            ( model
            , sendToFrontend clientId (SendMessage <| "Could not find document by slug (2b): " ++ slug)
            )

        Just doc ->
            ( model
            , Cmd.batch
                [ sendToFrontend clientId (SendDocument doc)
                ]
            )


signInAsGuestWithDoc model clientId path =
    let
        slug =
            String.dropLeft 3 path
    in
    case List.head (List.filter (\doc -> doc.slug == Just slug) model.documents) of
        Nothing ->
            ( model
            , sendToFrontend clientId (SendMessage <| "Could not find document by slug (2): " ++ slug)
            )

        Just doc ->
            ( model
            , Cmd.batch
                [ sendToFrontend clientId (SendDocument doc)
                , sendToFrontend clientId (SendMessage "Signed in as guest")
                , sendToFrontend clientId LoginGuest
                ]
            )


idMessage model =
    "ids: " ++ (List.map .id model.documents |> String.join ", ")
