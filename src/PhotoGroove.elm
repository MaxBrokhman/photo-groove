port module PhotoGroove exposing (Image, Model, Msg(..), Status(..), initialModel, main, photoDecoder, update, urlPrefix, view)

import Browser
import Html exposing (Attribute, Html, button, canvas, div, h1, h3, img, input, label, node, text)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onCheck, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


type alias Image =
    { url : String
    , size : Int
    , title : String
    }


type alias FilterOptions =
    { url : String
    , filters :
        List
            { name : String
            , amount : Float
            }
    }


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


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
    | Loaded (List Image) String
    | Errored String


type alias Model =
    { status : Status
    , activity : String
    , choosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , choosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
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
                    CheckedSize
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
        , onClick (ClickedImage thumb.url)
        ]
        []


type Msg
    = ClickedImage String
    | ClickedSurpriseMe
    | CheckedSize ThumbnailSize
    | GotRandomImage Image
    | GotImages (Result Http.Error (List Image))
    | SlideHue Int
    | SlideRipple Int
    | SlideNoise Int
    | GotActivity String


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotImages (Json.Decode.list photoDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        ClickedImage url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstImage :: images) _ ->
                    Random.uniform firstImage images
                        |> Random.generate GotRandomImage
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        CheckedSize size ->
            ( { model | choosenSize = size }, Cmd.none )

        GotRandomImage image ->
            applyFilters { model | status = selectUrl image.url model.status }

        SlideHue hue ->
            applyFilters { model | hue = hue }

        SlideRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlideNoise noise ->
            applyFilters { model | noise = noise }

        GotImages (Ok images) ->
            case images of
                first :: rest ->
                    applyFilters
                        { model | status = Loaded rest first.url }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotImages (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 10 }
                    , { name = "Ripple", amount = toFloat model.ripple / 10 }
                    , { name = "Noise", amount = toFloat model.noise / 10 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded images _ ->
            Loaded images url

        Loading ->
            status

        Errored _ ->
            status


viewLoaded : List Image -> String -> Model -> List (Html Msg)
viewLoaded images selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlideHue "Hue" model.hue
        , viewFilter SlideRipple "Ripple" model.ripple
        , viewFilter SlideNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.choosenSize) [ Small, Medium, Large ])
    , div
        [ id "thumbnails"
        , class (sizeToString model.choosenSize)
        ]
        (List.map
            (viewThumbnail selectedUrl)
            images
        )
    , canvas
        [ id "main-canvas"
        , class "large"
        ]
        []
    ]


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded images selectedUrl ->
                viewLoaded images selectedUrl model

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "10"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "initialazing Pasta  v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, initialCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "slideValue" ] int
        |> Json.Decode.map toMsg
        |> on "slide"
