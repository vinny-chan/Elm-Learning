module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, andThen, field, float, int, map5, string, succeed)
import RemoteData exposing (WebData)


type alias Product =
    { id : String
    , title : String
    , price : Float
    , stock : Int
    , thumbnail : String
    }


type alias Post =
    List Product


type alias Model =
    { post : WebData Post
    , searchId : String
    , isFiltering : Bool
    }


type Msg
    = FetchPosts
    | PostsReceived (WebData Post)
    | ChangeInput String
    | SearchProduct
    | ClearFilter


view : Model -> Html Msg
view model =
    div [ class "flex flex-col justify-center gap-y-2 w-2/6" ]
        [ div [ class "flex gap-2" ]
            [ input [ class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500", placeholder "Search by Product ID", value model.searchId, onInput ChangeInput ] []
            , button [ class "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm w-full sm:w-auto px-5 py-2.5 text-center dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800", onClick SearchProduct ] [ text "Search" ]
            , button [ class "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm w-full sm:w-auto px-5 py-2.5 text-center dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800", onClick ClearFilter ] [ text "Clear" ]
            ]
        , viewProductsOrError
            model
        ]


viewProductsOrError : Model -> Html Msg
viewProductsOrError model =
    case model.post of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success post ->
            let
                filteredPost =
                    if model.isFiltering then
                        List.filter (\l -> l.id == model.searchId) post

                    else
                        post
            in
            viewProducts filteredPost

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


productDecoder : Decoder Product
productDecoder =
    map5 Product
        (field "id" int |> andThen (\id -> succeed (String.fromInt id)))
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

        ChangeInput value ->
            ( { model | searchId = value }, Cmd.none )

        SearchProduct ->
            ( { model | isFiltering = True }, Cmd.none )

        ClearFilter ->
            ( { model | isFiltering = False }, Cmd.none )


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
    ( { post = RemoteData.Loading, searchId = "", isFiltering = False }, fetchPosts )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
