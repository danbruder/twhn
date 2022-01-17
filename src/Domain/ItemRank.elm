module Main exposing (comment, id, isComment, isJob, isStory, kids)


id : Item -> Int
id item =
    case item of
        Item__Story story ->
            story.id

        Item__Comment c ->
            c.id

        Item__Job c ->
            c.id


kids : Item -> List Int
kids item =
    case item of
        Item__Story story ->
            story.kids

        Item__Comment c ->
            c.kids

        Item__Job c ->
            []


comment : Item -> Maybe Comment
comment item =
    case item of
        Item__Comment c ->
            Just c

        _ ->
            Nothing


isJob : Item -> Bool
isJob item =
    case item of
        Item__Job c ->
            True

        _ ->
            False


isComment : Item -> Bool
isComment item =
    case item of
        Item__Comment c ->
            True

        _ ->
            False


isStory : Item -> Bool
isStory item =
    case item of
        Item__Story c ->
            True

        _ ->
            False
