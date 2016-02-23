open System

// Define your library scripting code here
type MarkdownDocument = list<MarkdownBlock>
and MarkdownBlock =
| Heading of int * MarkdownSpans
| Paragraph of MarkdownSpans
| CodeBlock of list<string>
and MarkdownSpans = list<MarkdownSpan>
and MarkdownSpan =
| Literal of string
| InlineCode of string
| Strong of MarkdownSpans
| Emphasis of MarkdownSpans
| HyperLink of MarkdownSpans * string

let rec parseInlineBody acc = function
    | '`'::rest ->
        Some(List.rev acc, rest)
    | c::chars ->
        parseInlineBody (c::acc) chars
    | [] -> None

let parseInline = function
    | '`'::chars -> 
            parseInlineBody [] chars
    | _ -> None

//"what's this `code` and" |> List.ofSeq |> parseInline

let toString chars = 
    String(chars |> Array.ofList)

let rec parseSpans acc chars = seq {
    let emitLiteral = seq {
        if acc <> [] then
            yield acc |> List.rev |> toString |> Literal
    }

    match parseInline chars, chars with
    | Some(body, chars), _ ->
        yield! emitLiteral
        yield body |> toString |> InlineCode
        yield! parseSpans [] chars
    | _, c::chars ->
        yield! parseSpans (c::acc) chars
    | _, [] ->
        yield! emitLiteral
}

parseSpans [] ("what's this `code` and" |> List.ofSeq)
|> List.ofSeq