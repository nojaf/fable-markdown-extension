module AdventExtension.Tests.Parser

open Fable.Import.Jest
open Fable.Import.Jest.Matchers
open AdventExtension
open AdventExtension.Parser


test "simple Jest test should be picked up" <| fun () ->
    "42" == "42"

test "basic fable font" <| fun () ->
    let input = "🐉vibes🐉"
    let parsed = Parser.parseText input
    parsed.status == true

test "no fable font" <| fun () ->
    let input = "fableless vibes"
    let parsed = Parser.parseText input
    parsed.status == true

test "fable logo is found" <| fun () ->
    let input = "Once upon a :fable:..."
    let parsed = Parser.parseText input
    
    parsed.status == true
    
    match parsed.value with
    | [| Text _; FableLogo logo; Text _|] ->
        logo.start.offset == 12
        logo.value.Length == 7
    | _ -> failwith "Expected logo in center"