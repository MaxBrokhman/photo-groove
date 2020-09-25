module Main exposing (main, view)

import Browser exposing (Document)
import Html exposing (Html, a, caption, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)


type alias Model =
    { page : Page }


type Page
    = Gallery
    | Folders
    | NotFound


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    let
        content =
            text "Some"
    in
    { title = "Photo Groove"
    , body =
        [ viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage { url, caption } =
            li [ classList [ ( "active", page == targetPage ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { page = Folders }, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
