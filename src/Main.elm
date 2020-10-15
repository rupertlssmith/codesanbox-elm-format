module Main exposing (main)

import Browser
import Html exposing (Html, article, button, div, h1, h2, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode
import RemoteData exposing (RemoteData(..))



-- The Entrypoint of Elm that is initialized from index.js


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


-- Initial model (initialState) and initial commands, if any


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = NotAsked }
    , Cmd.none
    )


-- Model is the state tree in JavaScript terms


type alias Model =
    { posts : RemoteData Http.Error (List Post)
    }


type alias Post =
    { id : Int
    , title : String
    , body : String
    }



-- Decoder. Run-time type checking of incoming JSON
-- to be sure it will convert to the correct Elm type.
-- Failing will result in a BadBody error (see the
-- errorToString function below)


postsDecoder : Json.Decode.Decoder (List Post)
postsDecoder =
    Json.Decode.list
        (Json.Decode.map3
            Post
            (Json.Decode.field "id" Json.Decode.int)
            (Json.Decode.field "title" Json.Decode.string)
            (Json.Decode.field "body" Json.Decode.string)
        )



-- Msgs are sent to the update function to update
-- state and do side-effetcs


type Msg
    = GotPostsResponse (RemoteData Http.Error (List Post))
    | GetPosts



-- Update, the only place where updating state can happen in Elm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPostsResponse data ->
            ( { model | posts = data }
            , Cmd.none
            )

        GetPosts ->
            ( { model | posts = Loading }
            , fetchPosts
            )


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson (RemoteData.fromResult >> GotPostsResponse) postsDecoder
        }



-- View


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ h1 [] [ text "Lorem Ipsum blog" ]
        , postView model.posts
        ]


postView : RemoteData Http.Error (List Post) -> Html Msg
postView postData =
    case postData of
        NotAsked ->
            div [ style "text-align" "center" ]
                [ div [] [ text "Not asked for posts yet" ]
                , button [ onClick GetPosts ] [ text "Fetch Posts" ]
                ]

        Loading ->
            text "Loading..."

        Failure err ->
            div []
                [ text "Something went wrong 😨"
                , text (errorToString err)
                ]

        Success postList ->
            div []
                (postList
                    |> List.map
                        (\post ->
                            blogArticle post
                        )
                )


blogArticle : Post -> Html msg
blogArticle post =
    article
        [ style "border" "1px solid darkgray"
        , style "margin" "1rem"
        , style "padding" "1rem"
        ]
        [ h2 [] [ text post.title ]
        , div [] [ text post.body ]
        ]



-- Just a convenient utility to pattern match the errors
-- and return relevant error data


errorToString : Http.Error -> String
errorToString httpError =
    case httpError of
        BadUrl string ->
            "Bad URL: " ++ string

        Timeout ->
            "It took too long to get a response"

        NetworkError ->
            "NetworkError"

        BadStatus int ->
            "BadStatus: " ++ String.fromInt int

        BadBody string ->
            "BadBody: " ++ string
