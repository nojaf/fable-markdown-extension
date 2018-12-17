module AdventExtension.Parser

open Fable.Parsimmon
open Fable.Core
open System.Text.RegularExpressions

type ParserResult = 
    | Text of string
    | FableFontText of NodeResult<string>
    | FableLogo of NodeResult<string>

// no /g flag
[<Emit("new RegExp($0)")>]
let createRegex pattern : Regex = jsNative

let dragonParser = Parsimmon.str "🐉"

let noDragonParser = 
    Parsimmon.regex (createRegex @"[^🐉]+")

let fableFontParser =
    Parsimmon.between dragonParser dragonParser noDragonParser
    |> Parsimmon.node "FableFont"
    |> Parsimmon.map (ParserResult.FableFontText)
    
let halfDragon =
    "🐉".ToCharArray()
    |> Array.head
    |> (string)

let noDragonsParser =
    Parsimmon.satisfy (fun token -> token <> halfDragon && token <> ":")
        |> Parsimmon.atLeastOneOrMany
        |> Parsimmon.concat
        |> Parsimmon.map (ParserResult.Text)
        
let fableLogoParser =
    Parsimmon.str ":fable:"
    |> Parsimmon.node "FableLogo"
    |> Parsimmon.map (FableLogo)

let parseText input : ParseResult<ParserResult array> =
    Parsimmon.choose [ 
        fableLogoParser
        noDragonsParser
        fableFontParser
    ] 
    |> fun parser -> parser.many().parse(input)