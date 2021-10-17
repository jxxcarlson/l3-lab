module LaTeX.Export.API exposing (..)

import Expression.ASTTools
import LaTeX.Export.Block
import Lang.Lang exposing (Lang)
import Markup.API


export : Lang -> String -> String
export language sourceText =
    let
        ast =
            sourceText
                |> String.lines
                |> Markup.API.parse language 0
                |> .ast

        titleString =
            Expression.ASTTools.getTitle ast |> Maybe.withDefault "Untitled"
    in
    ast |> LaTeX.Export.Block.render titleString
