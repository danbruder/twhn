module Util exposing (..)

import Html
import Html.Parser
import Html.Parser.Util
import Html.Styled


textHtml : String -> List (Html.Styled.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes
                |> List.map Html.Styled.fromUnstyled

        Err _ ->
            []
