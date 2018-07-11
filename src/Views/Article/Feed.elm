module Views.Article.Feed exposing (Model, Msg, init, selectTag, update, viewArticles, viewFeedSources)

{-| NOTE: This module has its own Model, view, and update. This is not normal!
If you find yourself doing this often, please watch <https://www.youtube.com/watch?v=DoA4Txr4GUs>

This is the reusable Article Feed that appears on both the Home page as well as
on the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}

import Browser.Dom as Dom
import Data.Article as Article exposing (Article)
import Data.Article.Feed exposing (Feed)
import Data.Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import Data.Article.Slug as ArticleSlug
import Data.Article.Tag as Tag exposing (Tag)
import Data.AuthToken exposing (AuthToken)
import Data.Session exposing (Session)
import Data.User
import Data.User.Username as Username exposing (Username)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Request.Article
import Task exposing (Task)
import Time
import Util exposing (onClickStopPropagation)
import Views.Article
import Views.Errors as Errors
import Views.Page
import Views.Spinner exposing (spinner)



-- MODEL --


type Model
    = Model InternalModel


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this module.
-}
type alias InternalModel =
    { errors : List String
    , feed : Feed
    , feedSources : FeedSources
    , activePage : Int
    , isLoading : Bool
    }


init : Session -> FeedSources -> Task Http.Error Model
init session feedSources =
    let
        source =
            FeedSources.selected feedSources

        toModel ( activePage, feed ) =
            Model
                { errors = []
                , activePage = activePage
                , feed = feed
                , feedSources = feedSources
                , isLoading = False
                }
    in
    source
        |> fetch (Maybe.map .token session.user) 1
        |> Task.map toModel



-- VIEW --


viewArticles : Time.Zone -> Model -> List (Html Msg)
viewArticles timeZone (Model { activePage, feed, feedSources }) =
    List.map (Views.Article.view ToggleFavorite timeZone) feed.articles
        ++ [ pagination activePage feed (FeedSources.selected feedSources) ]


viewFeedSources : Model -> Html Msg
viewFeedSources (Model { feedSources, isLoading, errors }) =
    let
        errorsHtml =
            Errors.view DismissErrors errors

        spinnerHtml =
            if isLoading then
                spinner

            else
                text ""
    in
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewFeedSource False) (FeedSources.before feedSources)
            , [ viewFeedSource True (FeedSources.selected feedSources) ]
            , List.map (viewFeedSource False) (FeedSources.after feedSources)
            , [ errorsHtml, spinnerHtml ]
            ]


viewFeedSource : Bool -> Source -> Html Msg
viewFeedSource isSelected source =
    li [ class "nav-item" ]
        [ a
            [ classList [ ( "nav-link", True ), ( "active", isSelected ) ]
            , href "javascript:void(0);"
            , onClick (SelectFeedSource source)
            ]
            [ text (sourceName source) ]
        ]


selectTag : Maybe AuthToken -> Tag -> Cmd Msg
selectTag maybeAuthToken tagName =
    let
        source =
            TagFeed tagName
    in
    source
        |> fetch maybeAuthToken 1
        |> Task.attempt (FeedLoadCompleted source)


sourceName : Source -> String
sourceName source =
    case source of
        YourFeed ->
            "Your Feed"

        GlobalFeed ->
            "Global Feed"

        TagFeed tagName ->
            "#" ++ Tag.toString tagName

        FavoritedFeed username ->
            "Favorited Articles"

        AuthorFeed username ->
            "My Articles"


limit : Source -> Int
limit feedSource =
    case feedSource of
        YourFeed ->
            10

        GlobalFeed ->
            10

        TagFeed tagName ->
            10

        FavoritedFeed username ->
            5

        AuthorFeed username ->
            5


pagination : Int -> Feed -> Source -> Html Msg
pagination activePage feed feedSource =
    let
        articlesPerPage =
            limit feedSource

        totalPages =
            ceiling (toFloat feed.articlesCount / toFloat articlesPerPage)
    in
    if totalPages > 1 then
        List.range 1 totalPages
            |> List.map (\page -> pageLink page (page == activePage))
            |> ul [ class "pagination" ]

    else
        Html.text ""


pageLink : Int -> Bool -> Html Msg
pageLink page isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , href "javascript:void(0);"
            , onClick (SelectPage page)
            ]
            [ text (String.fromInt page) ]
        ]



-- UPDATE --


type Msg
    = DismissErrors
    | SelectFeedSource Source
    | FeedLoadCompleted Source (Result Http.Error ( Int, Feed ))
    | ToggleFavorite (Article ())
    | FavoriteCompleted (Result Http.Error (Article ()))
    | SelectPage Int


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg (Model internalModel) =
    updateInternal session msg internalModel
        |> Tuple.mapFirst Model


updateInternal : Session -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal session msg model =
    case msg of
        DismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        SelectFeedSource source ->
            source
                |> fetch (Maybe.map .token session.user) 1
                |> Task.attempt (FeedLoadCompleted source)
                |> Tuple.pair { model | isLoading = True }

        FeedLoadCompleted source (Ok ( activePage, feed )) ->
            ( { model
                | feed = feed
                , feedSources = FeedSources.select source model.feedSources
                , activePage = activePage
                , isLoading = False
              }
            , Cmd.none
            )

        FeedLoadCompleted _ (Err error) ->
            ( { model
                | errors = model.errors ++ [ "Server error while trying to load feed" ]
                , isLoading = False
              }
            , Cmd.none
            )

        ToggleFavorite article ->
            case session.user of
                Nothing ->
                    ( { model | errors = model.errors ++ [ "You are currently signed out. You must sign in to favorite articles." ] }
                    , Cmd.none
                    )

                Just user ->
                    Request.Article.toggleFavorite article user.token
                        |> Http.send FavoriteCompleted
                        |> Tuple.pair model

        FavoriteCompleted (Ok article) ->
            let
                feed =
                    model.feed

                newFeed =
                    { feed | articles = List.map (replaceArticle article) feed.articles }
            in
            ( { model | feed = newFeed }, Cmd.none )

        FavoriteCompleted (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to favorite article." ] }
            , Cmd.none
            )

        SelectPage page ->
            let
                source =
                    FeedSources.selected model.feedSources
            in
            source
                |> fetch (Maybe.map .token session.user) page
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt (FeedLoadCompleted source)
                |> Tuple.pair model


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())


fetch : Maybe AuthToken -> Int -> Source -> Task Http.Error ( Int, Feed )
fetch authToken page feedSource =
    let
        defaultListConfig =
            Request.Article.defaultListConfig

        articlesPerPage =
            limit feedSource

        offset =
            (page - 1) * articlesPerPage

        listConfig =
            { defaultListConfig | offset = offset, limit = articlesPerPage }

        task =
            case feedSource of
                YourFeed ->
                    case authToken of
                        Just token ->
                            let
                                defaultFeedConfig =
                                    Request.Article.defaultFeedConfig

                                feedConfig =
                                    { defaultFeedConfig | offset = offset, limit = articlesPerPage }
                            in
                            Request.Article.feed feedConfig token
                                |> Http.toTask

                        Nothing ->
                            Http.BadUrl "You need to be signed in to view your feed."
                                |> Task.fail

                GlobalFeed ->
                    Request.Article.list listConfig authToken
                        |> Http.toTask

                TagFeed tagName ->
                    Request.Article.list { listConfig | tag = Just tagName } authToken
                        |> Http.toTask

                FavoritedFeed username ->
                    Request.Article.list { listConfig | favorited = Just username } authToken
                        |> Http.toTask

                AuthorFeed username ->
                    Request.Article.list { listConfig | author = Just username } authToken
                        |> Http.toTask
    in
    task
        |> Task.map (\feed -> ( page, feed ))


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if newArticle.slug == oldArticle.slug then
        newArticle

    else
        oldArticle
