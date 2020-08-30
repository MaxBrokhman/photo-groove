module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Random


type alias Image =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { photos : List Image
    , selectedUrl : String
    , choosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , choosenSize = Medium
    }


photosArray : Array Image
photosArray =
    Array.fromList initialModel.photos


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photosArray - 1)


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
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe
    | ClickedSize ThumbnailSize
    | GotSelectedIndex Int


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photosArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ClickedSize size ->
            ( { model | choosenSize = size }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.choosenSize) [ Small, Medium, Large ])
        , div
            [ id "thumbnails"
            , class (sizeToString model.choosenSize)
            ]
            (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
