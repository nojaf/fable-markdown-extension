module AdventExtension.Extension

open Fable.Import
open Fable.Core.JsInterop
open Fable.Import.vscode
open AdventExtension.Parser
open Fable.Parsimmon

let createFontOptions color backgroundColor fontWeight : DecorationRenderOptions =
    jsOptions (fun options ->
        options.color <- Some color
        options?fontWeight <- fontWeight
        options.backgroundColor <- backgroundColor
    )

// Parsimmon ranges are 1-based, VSCode works with 0-based indexing.
let nodeResultToRange<'t> (nr: NodeResult<'t>) =
    let zeroBasedFloat r = r - 1 |> (float)
    vscode.Range(zeroBasedFloat nr.start.line,
                 zeroBasedFloat nr.start.column,
                 zeroBasedFloat nr.``end``.line,
                 zeroBasedFloat nr.``end``.column)

let activate (context: ExtensionContext) =
    let fableFontStyle = window.createTextEditorDecorationType(createFontOptions "#87c5fd" (Some "white") None)
    let fableLogoStyle = window.createTextEditorDecorationType(createFontOptions "#87c5fd" None (Some "bold"))

    // Capture the active editor.
    let mutable activeEditor = window.activeTextEditor

    let updateDecorations() =
        activeEditor
        |> Option.iter (fun aEditor ->
            if aEditor.document.languageId = "markdown" then
                let text = aEditor.document.getText()
                let parsed = Parser.parseText text
                
                if parsed.status then
                    parsed.value
                    |> Array.map (fun node ->
                        match node with
                        | ParserResult.FableLogo logo ->
                            Some (fableLogoStyle, nodeResultToRange logo)
                            
                        | ParserResult.FableFontText fableText ->
                            Some (fableFontStyle, nodeResultToRange fableText)
                            
                        | _ ->
                            None
                                
                    )
                    |> Array.choose id
                    |> Array.groupBy (fun (style,_) -> style.key)
                    |> Array.iter (fun (styleKey, ranges) ->
                        let r =
                            Array.map snd ranges
                            |> ResizeArray
                        
                        let style = if fableFontStyle.key = styleKey then fableFontStyle else fableLogoStyle
                        
                        aEditor.setDecorations(style, !^ r)
                    )
        )

    let mutable timeoutKey = None;
    let triggerUpdateDecorations _ =
        timeoutKey
        |> Option.iter (JS.clearTimeout)

        timeoutKey <- Some (JS.setTimeout updateDecorations 500)

    // User is typing
    window.onDidChangeActiveTextEditor.Invoke((fun editor ->
        activeEditor <- Some editor
        triggerUpdateDecorations()
        null
    ))
    |> context.subscriptions.Add
    
    // Change of document
    workspace.onDidChangeTextDocument.Invoke((fun event ->
        match activeEditor with
        | Some aEditor when (event.document = aEditor.document) ->
            triggerUpdateDecorations()
        | _ -> ()
        null
    ))
    |> context.subscriptions.Add

    activeEditor
    |> Option.iter (triggerUpdateDecorations)

    // Add render plugin
    createObj [
        "extendMarkdownIt" ==> (fun md -> md?``use``(MarkdownPlugin.fableMarkdownPlugin))
    ]

    