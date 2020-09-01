module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Http exposing (Expect)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (initialModel, urlPrefix)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


initialModel =
    PhotoGroove.initialModel


decoderTest : Test
decoderTest =
    fuzz2 string int "Title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")


sliders : Test
sliders =
    describe "Sliders set the desired fields in the Model"
        [ testSlider "SlideHue" PhotoGroove.SlideHue .hue
        , testSlider "SlideRipple" PhotoGroove.SlideRipple .ripple
        , testSlider "SliderNoise" PhotoGroove.SlideNoise .noise
        ]


testSlider : String -> (Int -> PhotoGroove.Msg) -> (PhotoGroove.Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> PhotoGroove.update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount


noImagesNoThumbnails : Test
noImagesNoThumbnails =
    test "No thumbnails render when there are no images" <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


imageFromUrl : String -> PhotoGroove.Image
imageFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = PhotoGroove.Loaded (List.map imageFromUrl urls) "" }
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "Clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                images =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map imageFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = PhotoGroove.Loaded images "" }
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (PhotoGroove.ClickedImage url)
