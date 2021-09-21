module Markdown.Line exposing (classify)

import Common.BlockParser as BP
import Common.Debug exposing (debug2, debug3)
import Common.Library.ParserTools as ParserTools
import Common.Line as Line
import Parser exposing ((|.), (|=), Parser)


classify : BP.ScannerType -> String -> { indent : Int, lineType : Line.LineType, content : String }
classify scannerType str =
    let
        leadingSpaces =
            Line.countLeadingSpaces str

        _ =
            debug2 "scannerType" scannerType

        lineType_ : Line.LineType
        lineType_ =
            case scannerType of
                BP.NormalScan ->
                    lineType (String.dropLeft leadingSpaces str) |> debug3 "classify, lineType"

                BP.VerbatimScan _ ->
                    Line.OrdinaryLine

        nibble str_ =
            String.dropLeft (String.length (ParserTools.nibble str_) + 1) str_
    in
    { indent = leadingSpaces, lineType = lineType_, content = nibble str |> debug3 "classify, str" }


lineType : String -> Line.LineType
lineType str =
    let
        _ =
            debug3 "lineType, line" str
    in
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf
        [ beginCodeBlockParser
        , beginMathBlockParser
        , beginItemParser
        , beginQuotationBlockParser
        , Line.ordinaryLineParser []
        , Line.emptyLineParser
        ]


beginItemParser : Parser Line.LineType
beginItemParser =
    (Parser.succeed String.slice
        |. Parser.symbol "-"
    )
        |> Parser.map (\_ -> Line.BeginBlock "item")


beginMathBlockParser : Parser Line.LineType
beginMathBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "$$"
    )
        |> Parser.map (\_ -> Line.BeginVerbatimBlock '$' "math")


beginCodeBlockParser : Parser Line.LineType
beginCodeBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "```"
    )
        |> Parser.map (\_ -> Line.BeginVerbatimBlock '`' "code")


beginQuotationBlockParser : Parser Line.LineType
beginQuotationBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol ">"
    )
        |> Parser.map (\_ -> Line.BeginBlock "quotation")
