module Article.FeedSources exposing (FeedSources, Source(..), after, before, fromLists, select, selected)

import Article
import Article.Feed as Feed
import Article.Tag as Tag exposing (Tag)
import Username exposing (Username)



-- TYPES


type FeedSources
    = FeedSources
        { before : List Source
        , selected : Source
        , after : List Source
        }


type Source
    = YourFeed
    | GlobalFeed
    | TagFeed Tag
    | FavoritedFeed Username
    | AuthorFeed Username



-- BUILDING


fromLists : Source -> List Source -> FeedSources
fromLists selectedSource afterSources =
    FeedSources
        { before = []
        , selected = selectedSource
        , after = afterSources
        }



-- SELECTING


select : Source -> FeedSources -> FeedSources
select selectedSource (FeedSources sources) =
    let
        -- By design, tags can only be included if they're selected.
        removeSelectedAndTags : List Source -> List Source
        removeSelectedAndTags list =
            List.filter (shouldKeepSelected selectedSource) list
    in
    FeedSources
        { before = removeSelectedAndTags sources.before
        , selected = selectedSource
        , after = removeSelectedAndTags sources.after
        }


shouldKeepSelected : Source -> Source -> Bool
shouldKeepSelected selectedSource currentSource =
    -- I'm enumerating each of these because I want a MISSING PATTERNS error
    -- if I add a new one. This way I can't forget to handle any new cases
    -- that come up in the future.
    case selectedSource of
        TagFeed _ ->
            False

        YourFeed ->
            currentSource /= selectedSource

        GlobalFeed ->
            currentSource /= selectedSource

        FavoritedFeed _ ->
            currentSource /= selectedSource

        AuthorFeed _ ->
            currentSource /= selectedSource



-- ACCESSING


selected : FeedSources -> Source
selected (FeedSources record) =
    record.selected


before : FeedSources -> List Source
before (FeedSources record) =
    record.before


after : FeedSources -> List Source
after (FeedSources record) =
    record.after
