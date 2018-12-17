module AdventExtension.Parser

open Fable.Parsimmon

type ParserResult = 
    | Text of string
    | FableFontText of NodeResult<string>
    | FableLogo of NodeResult<string>

let parseText input : ParseResult<ParserResult array> = failwith "Nothing here yet."