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


type alias Model =
    { selectedImageUrl : Maybe String
    , images : Dict String Image
    }


initialModel : Model
initialModel =
    { selectedImageUrl = Nothing
    , images = Dict.empty
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



-- modelDecoder =
--     Decode.succeed initialModel


modelDecoder =
    Decode.succeed
        { selectedImageUrl = Just "trevi"
        , images =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        }


type Msg
    = ClickedImage String
    | GotInitialModel (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
        [ div [ class "selected-photo" ] [ selectedImage ] ]


type alias Image =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


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
