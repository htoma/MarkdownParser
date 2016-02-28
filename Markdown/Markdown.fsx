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

let toString chars = 
    String(chars |> Array.ofList)

let (|StartsWith|_|) prefix input = 
    let rec loop = function
        | p::prefix, r::rest when p=r ->
            loop(prefix, rest)
        | [], rest ->
            Some(rest)
        | _ -> None
    loop(prefix, input)

let rec parseBracketedBody closing acc = function
    | StartsWith closing (rest) ->
        Some(List.rev acc, rest)
    | c::chars ->
        parseBracketedBody closing (c::acc) chars
    | _ -> None

let parseBracketed opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim

let rec parseSpans acc chars = seq {
    let emitLiteral = seq {
      if acc <> [] then
        yield acc |> List.rev |> toString |> Literal }
    
    match chars with
    | Delimited ['`'] (body, chars) ->
        yield! emitLiteral
        yield InlineCode(toString body)
        yield! parseSpans [] chars
    | Delimited ['*'; '*' ] (body, chars)
    | Delimited ['_'; '_' ] (body, chars) ->
        yield! emitLiteral
        yield Strong(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | Delimited ['*' ] (body, chars)
    | Delimited ['_' ] (body, chars) ->
        yield! emitLiteral
        yield Emphasis(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | c::chars ->
        yield! parseSpans (c::acc) chars
    | [] ->
        yield! emitLiteral }

"what's this `code` *and* or _other_" |> List.ofSeq
|> parseSpans []
|> List.ofSeq

