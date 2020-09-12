module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import PhotoGroove exposing (Image, urlPrefix)


type Folder
    = Folder
        { name : String
        , imageUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }


type alias Model =
    { selectedImageUrl : Maybe String
    , images : Dict String Image
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedImageUrl = Nothing
    , images = Dict.empty
    , root =
        Folder
            { name = "Loading..."
            , imageUrls = []
            , subfolders = []
            , expanded = True
            }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\images root ->
            { images = images, root = root, selectedImageUrl = Nothing }
        )
        modelImagesDecoder
        folderDecoder


type Msg
    = ClickedImage String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )

        ClickedImage url ->
            ( { model | selectedImageUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        imageByUrl : String -> Maybe Image
        imageByUrl url =
            Dict.get url model.images

        selectedImage : Html Msg
        selectedImage =
            case Maybe.andThen imageByUrl model.selectedImageUrl of
                Just image ->
                    viewSelectedImage image

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedImage ]
        ]


type alias Image =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewImage : String -> Html Msg
viewImage url =
    div [ class "photo", onClick (ClickedImage url) ]
        [ text url ]


viewSelectedImage : Image -> Html Msg
viewSelectedImage image =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text image.title ]
        , img [ src (urlPrefix ++ "photos/" ++ image.url ++ "/full") ] []
        , span [] [ text (String.fromInt image.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedImage image.relatedUrls)
        ]


viewRelatedImage : String -> Html Msg
viewRelatedImage url =
    img
        [ src url
        , class "related-photo"
        , onClick (ClickedImage url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map viewImage folder.imageUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ]
            [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


type FolderPath
    = End
    | Subfolder Int FolderPath


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }


type alias JsonImage =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


jsonImageDecoder : Decoder JsonImage
jsonImageDecoder =
    Decode.succeed JsonImage
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)


finishImage : ( String, JsonImage ) -> ( String, Image )
finishImage ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonImage ) -> Dict String Image
fromPairs pairs =
    pairs
        |> List.map finishImage
        |> Dict.fromList


imagesDecoder : Decoder (Dict String Image)
imagesDecoder =
    Decode.keyValuePairs jsonImageDecoder
        |> Decode.map fromPairs


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" imagesDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))


folderFromJson : String -> Dict String Image -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , subfolders = subfolders
        , imageUrls = Dict.keys photos
        }


modelImagesDecoder : Decoder (Dict String Image)
modelImagesDecoder =
    Decode.succeed modelImagesFromJson
        |> required "photos" imagesDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list modelImagesDecoder))


modelImagesFromJson :
    Dict String Image
    -> List (Dict String Image)
    -> Dict String Image
modelImagesFromJson folderImages subfolderImages =
    List.foldl Dict.union folderImages subfolderImages


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
