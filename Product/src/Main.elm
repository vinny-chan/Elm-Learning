module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode exposing (Decoder, field, float, int, map4, string)
import RemoteData exposing (WebData)


type alias Product =
    { title : String
    , price : Float
    , stock : Int
    , thumbnail : String
    }


type alias Post =
    List Product


type alias Model =
    { post : WebData Post
    }


view : Model -> Html Msg
view model =
    div [ class "flex justify-center" ]
        [ viewProductsOrError model
        ]


viewProductsOrError : Model -> Html Msg
viewProductsOrError model =
    case model.post of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success posts ->
            viewProducts posts

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewError : String -> Html Msg
viewError errorMessage =
    div []
        [ h3 [] [ text "Unable to fetch data at this time." ]
        , text ("Error: " ++ errorMessage)
        ]


viewProducts : Post -> Html Msg
viewProducts products =
    div [ class "flex flex-col gap-y-2" ]
        (List.map viewProduct products)


viewProduct : Product -> Html Msg
viewProduct { title, price, stock, thumbnail } =
    div
        [ class "flex flex-col items-center bg-white rounded-lg border shadow-md md:flex-row md:max-w-xl dark:border-gray-700 dark:bg-gray-800 p-5 gap-x-5" ]
        [ img [ class "w-52 h-52", src thumbnail ] []
        , div [ class "flex flex-col items-start" ]
            [ p [ class "mb-3 font-normal text-gray-700 dark:text-gray-400 text-left" ] [ text ("Product Name: " ++ title) ]
            , p [ class "mb-3 font-normal text-gray-700 dark:text-gray-400 text-left" ] [ text ("Price: $" ++ (price |> String.fromFloat)) ]
            , p [ class "mb-3 font-normal text-gray-700 dark:text-gray-400 text-left" ] [ text ("Quantity Remaining: " ++ (stock |> String.fromInt)) ]
            ]
        ]


type Msg
    = FetchPosts
    | PostsReceived (WebData Post)


productDecoder : Decoder Product
productDecoder =
    map4 Product
        (field "title" string)
        (field "price" float)
        (field "stock" int)
        (field "thumbnail" string)


postDecoder : Decoder Post
postDecoder =
    field "products" (Json.Decode.list productDecoder)


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "https://dummyjson.com/products"
        , expect =
            postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostsReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | post = RemoteData.Loading }, fetchPosts )

        PostsReceived response ->
            ( { model | post = response }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd Msg )
init _ =
    ( { post = RemoteData.Loading }, fetchPosts )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
