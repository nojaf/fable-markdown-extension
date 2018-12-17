module AdventExtension.MarkdownPlugin

open MarkdownItModule
open Fable.Core
open AdventExtension.Parser

[<Emit("new $0.Token($1,$2,$3)")>]
let createToken state name tag nesting: Token = jsNative

[<Emit("$0[$1] = $2")>]
let addRenderRule rules name fn: unit = jsNative

let fableMarkdownPlugin (md: MarkdownIt) (options: Options) =
    // Phase one: replace text tokens to custom tokens
    let replaceTokens (state: State) =
        let parseTextToken (parentToken: Token) (textToken: Token) = 
            let textContent = textToken.content
            let parsedContent = parseText textContent
            let updatedChildren =
                if not parsedContent.status then
                    Array.empty
                else
                    parsedContent.value
                    |> Array.map (fun node ->
                        match node with
                        | ParserResult.Text text ->
                            let t = createToken state "text" "" 0
                            t.content <- text
                            t
                        
                        | ParserResult.FableLogo _ ->
                            createToken state "fable-logo" "img" 0
                        
                        | ParserResult.FableFontText fontText ->
                            let t = createToken state "fable-font" "span" 0
                            t.content <- fontText.value.Replace("🐉", "")
                            t
                    )
                
            
            parentToken.children <- new ResizeArray<Token>(updatedChildren)
            
        let rec parseToken (parentToken: Token) (token: Token) =
            if token.children <> null && not(Seq.isEmpty token.children) then
                token.children
                |> Seq.iter (fun childToken ->
                    if childToken.``type`` = "inline" then
                        parseToken token childToken
                    else
                        parseTextToken token childToken 
                )

            if token.``type`` = "text" then
                parseTextToken parentToken token
        
            
        state.tokens
        |> Seq.filter (fun t -> t.``type`` = "inline" && t.children <> null && not (Seq.isEmpty t.children))
        |> Seq.iter (fun token ->
            token.children
            |> Seq.iter (parseToken token)
        )
        
    md.core.ruler.push("fable", replaceTokens)

    // phase two: render something else when custom token
    let renderFableLogo tokens idx = "<img class='fable-logo' src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAhCAYAAADOHBvaAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAAYdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjEuMWMqnEsAAALfSURBVFhHvZdNaBNBFMfjV0GNNrtpbItthVZabBTEkxEqUlREEEFFsYeAJz+KiHgo1kPBk4i12d0mukkFrVVri6BCFVFQBLEH682bH3gQESxUzE33/X27mcYkrsl+lP7gsTuzO/PbN7M7mQTmGwBWzDvexDqWiDPPOBJLOqqlQYqKYq6s0ris0r7aFK2KKNQiK3RA0igW6EOVuK0sjjMOqdQhKfSUhWpYoW4+/yqrQGnw9R/8EKMhjbpqLlO92bahn5YGxrDI6khQUVydQjN3dJYjbYWNrFzwA/7kh8mWTs//xX1YyI3Oy4rxi4W/ZQUzPLSGXecVQ6FPbFkgerawF/NNcsLIsPiGnKAd1lAxQYUiXHfPtnObMDPlGFjZT7LVbwG24hqNWqUkNYpiEaEEddlJ/oY5KvSKszzJ0ySJZv9gn3EZzJesRPSWJRf52Msv3d7gJdSIW8viWlzHw10o5qEcFpdc4VpsIqnGu7xcoY9eFhRPYh7uE0VZ80tYO0zLxWVHeBKbiwHP6eNCOWf+nSPNX8ERaYBikUHUcc9Fn1AhrsXtY6hq1en01lsUb8sY79emgWYdaLrKcQVYw0ezbNa36JRt1ul1Y5Lionke9xlzFm0ZzHSOAvEJ4NxLQJsCLkwCPS+Ao0+Aww+B3ePAlhEgeg1oSLlZucqwboim3IkB82sQzS08ivHGjXh1kqbNJVg0t3Av5g5a0zTtRBwbMbLRIZps0mm/aJ3HU8btgwh2jtKm+CPaeegB9XbcBjbfBDZeB7bdoZ6uCdqw6645tHP4VpdSvISSYX1GDvAtZuH2WTEvn89EdUX8i3nLkxcrdFBUV8S3OKxRd05MnwPPsVhUV2QuMuafRBZrdEpUOcK3mOf1Pg/xt3ody0SVI/xnrNIHlp8RRcf4Eq/QKCwpxpfZPZkbfIlDCu3hTfwxUXSFX/Fxr39nfImlBK0Xp67xLi6zu3BCXizNMzkxAn8Apfg0ON9n078AAAAASUVORK5CYII=' />"
    
    let renderFableText (tokens: Token array) (idx: int) =
        let token = tokens.[idx]
        sprintf "<span class='fable'>%s</span>" (token.content.Replace("🐉", ""))
    
    addRenderRule md.renderer.rules "fable-logo" (new System.Func<Token array,int,string>(fun tokens idx -> renderFableLogo tokens idx))
    addRenderRule md.renderer.rules "fable-font" (new System.Func<Token array,int,string>(fun tokens idx -> renderFableText tokens idx))