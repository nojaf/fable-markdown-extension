module AdventExtension.Tests.MarkdownPlugin

open Fable.Import.Jest
open AdventExtension.MarkdownPlugin

jest.unmock("markdown-it")

let md = MarkdownItModule.Exports.Invoke().``use``(fableMarkdownPlugin)

test "fable img is rendered" <| fun () ->
    let input = ":fable:"
    let parsed = md.render input
    printfn "parsed %s" parsed
    expect.Invoke(parsed.StartsWith("<p><img class='fable-logo' src")).toBeTruthy()

test "fable font is rendered" <| fun () ->
    let input = "Over the seas 🐉we shall rise🐉!"
    let parsed = md.render input
    expect.Invoke(parsed.Contains("<span class='fable'>we shall rise</span>")).toBeTruthy()
    
test "Existing markdown is not impacted" <| fun () ->
    let input =
        """
# Fable

:fable: logo

- One
- Two
- Three

People say 🐉huuuuuuuuup🐉 where I live.
"""
    let parsed = md.render input
    printfn "parsed %s" parsed