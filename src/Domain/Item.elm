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

        Item__Comment comment ->
            comment.id
