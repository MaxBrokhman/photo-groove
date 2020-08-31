module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Random


type alias Image =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Image
photoDecoder =
    succeed Image
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "size" int
        |> optional "title" string "(untitled)"


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded Image (List Image) String
    | Errored String


type alias Model =
    { status : Status
    , choosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , choosenSize = Medium
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser checkedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onCheck
                (\checked ->
                    ClickedSize
                        (if checked then
                            size

                         else
                            checkedSize
                        )
                )
            , checked (size == checkedSize)
            ]
            []
        , text (sizeToString size)
        ]


viewThumbnail : String -> Image -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe
    | ClickedSize ThumbnailSize
    | GotRandomPhoto Image
    | GotImages (Result Http.Error (List Image))


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotImages (Json.Decode.list photoDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded firstImage images _ ->
                    Random.uniform firstImage images
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | choosenSize = size }, Cmd.none )

        GotRandomPhoto image ->
            ( { model | status = selectUrl image.url model.status }, Cmd.none )

        GotImages (Ok images) ->
            case images of
                first :: rest ->
                    ( { model | status = Loaded first rest first.url }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotImages (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded firstImage images _ ->
            Loaded firstImage images url

        Loading ->
            status

        Errored _ ->
            status


viewLoaded : List Image -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl choosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser choosenSize) [ Small, Medium, Large ])
    , div
        [ id "thumbnails"
        , class (sizeToString choosenSize)
        ]
        (List.map
            (viewThumbnail selectedUrl)
            photos
        )
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded _ images selectedUrl ->
                viewLoaded images selectedUrl model.choosenSize

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
