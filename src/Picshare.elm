module Picshare exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


type alias Feed =
    List Photo


type Msg
    = ToggleLike Id
    | UpdateComment Id String
    | SaveComment Id
    | LoadFeed (Result Http.Error Feed)


type alias Id =
    Int


type alias Photo =
    { id : Id, url : String, caption : String, liked : Bool, comments : List String, newComment : String }


type alias Model =
    { feed : Maybe Feed
    , error : Maybe Http.Error
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "id" int
        |> required "url" string
        |> required "caption" string
        |> required "liked" bool
        |> required "comments" (list string)
        |> hardcoded ""


initialModel =
    { feed = Nothing, error = Nothing }


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed"
        , expect = Http.expectJson LoadFeed (list photoDecoder)
        }


viewComment : String -> Html Msg
viewComment comment =
    li [ class "comment" ]
        [ strong []
            [ text comment
            ]
        ]


viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""

        _ ->
            div []
                [ ul [ class "comments" ] (List.map viewComment comments)
                ]


viewComments : Photo -> Html Msg
viewComments photo =
    div [ class "comments-info" ]
        [ viewCommentList photo.comments
        , form
            [ class "new-comment"
            , onSubmit (SaveComment photo.id)
            ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , onInput (UpdateComment photo.id)
                , value photo.newComment
                ]
                []
            , button [ disabled (String.isEmpty photo.newComment) ] [ text "Save" ]
            ]
        ]


viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
    let
        likedClass =
            if photo.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "detailed-photo" ]
        [ img [ src photo.url ] []
        , div [ class "photo-info" ]
            [ div [ class "like-button" ]
                [ i
                    [ class "fa fa-2x"
                    , class likedClass
                    , onClick (ToggleLike photo.id)
                    ]
                    []
                ]
            , h2 [ class "caption" ] [ text photo.caption ]
            , viewComments photo
            ]
        ]


saveNewComment : Photo -> Photo
saveNewComment photo =
    let
        comment =
            String.trim photo.newComment
    in
    case comment of
        "" ->
            photo

        _ ->
            { photo | comments = photo.comments ++ [ comment ], newComment = "" }


toggleLike : Photo -> Photo
toggleLike photo =
    { photo | liked = not photo.liked }


updatePhotoById : (Photo -> Photo) -> Id -> Feed -> Feed
updatePhotoById updatePhoto id feed =
    List.map
        (\photo ->
            if photo.id == id then
                updatePhoto photo

            else
                photo
        )
        feed


updateComment : String -> Photo -> Photo
updateComment comment photo =
    { photo | newComment = comment }


updateFeed : (Photo -> Photo) -> Id -> Maybe Feed -> Maybe Feed
updateFeed updatePhoto id maybeFeed =
    Maybe.map (updatePhotoById updatePhoto id) maybeFeed


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadBody _ ->
            """
            Sorry, we couldn't process your feed at this time.
            We're working on it.
        """

        _ ->
            """
            Sorry, we couldn't load your feed at this time.
            Please try again later.
            """


viewContent : Model -> Html Msg
viewContent model =
    case model.error of
        Just error ->
            div [ class "feed-error" ] [ text (errorMessage error) ]

        Nothing ->
            viewFeed model.feed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike id ->
            ( { model | feed = updateFeed toggleLike id model.feed }, Cmd.none )

        UpdateComment id comment ->
            ( { model | feed = updateFeed (updateComment comment) id model.feed }, Cmd.none )

        SaveComment id ->
            ( { model | feed = updateFeed saveNewComment id model.feed }, Cmd.none )

        LoadFeed (Ok feed) ->
            ( { model | feed = Just feed }, Cmd.none )

        LoadFeed (Err error) ->
            ( { model | error = Just error }, Cmd.none )


viewFeedPlaceholder : Int -> Html Msg
viewFeedPlaceholder _ =
    div [ class "feed-placeholder" ] [ text "Loading..." ]


viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed =
    case maybeFeed of
        Just feed ->
            div [ class "feeds" ] (List.map viewDetailedPhoto feed)

        Nothing ->
            div [ class "feed-placeholder-list" ]
                (List.map viewFeedPlaceholder [ 1, 2, 3 ])


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ]
            ]
        , div [ class "content-flow" ]
            [ viewContent model
            ]
        ]


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, fetchFeed )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
