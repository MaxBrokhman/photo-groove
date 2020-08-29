module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)


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


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photosArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSurpriseMe ->
            { model | selectedUrl = "2.jpeg" }

        ClickedSize size ->
            { model | choosenSize = size }


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


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
