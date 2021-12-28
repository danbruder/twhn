module Domain.Item exposing (..)

import Domain.Comment exposing (Comment)
import Domain.Story exposing (Story)
import Url exposing (Url)


type Item
    = Item__Story Story
    | Item__Comment Comment


id : Item -> Int
id item =
    case item of
        Item__Story story ->
            story.id

        Item__Comment c ->
            c.id


kids : Item -> List Int
kids item =
    case item of
        Item__Story story ->
            story.kids

        Item__Comment c ->
            c.kids


comment : Item -> Maybe Comment
comment item =
    case item of
        Item__Story _ ->
            Nothing

        Item__Comment c ->
            Just c


isComment : Item -> Bool
isComment item =
    case item of
        Item__Story _ ->
            False

        Item__Comment c ->
            True


isStory : Item -> Bool
isStory item =
    case item of
        Item__Comment _ ->
            False

        Item__Story c ->
            True
