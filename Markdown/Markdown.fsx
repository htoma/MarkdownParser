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
| HardLineBreak

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

let (|Bracketed|_|) opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|LineBreak|_|) = function
    | StartsWith ['\r';'\n'] rest
    | StartsWith ['\n';'\r'] rest
    | StartsWith ['\r'] rest
    | StartsWith ['\n'] rest ->
        Some(rest)
    | _ -> None

let rec parseSpans acc chars = seq {
    let emitLiteral = seq {
      if acc <> [] then
        yield acc |> List.rev |> toString |> Literal }
    
    match chars with
    | Bracketed ['['] [']'] (body,chars) ->
        match chars with
        | Bracketed ['('] [')'] (inner,rest) ->
            yield! emitLiteral
            yield HyperLink(parseSpans [] body |> List.ofSeq, toString inner)
            yield! parseSpans [] rest
        | _ -> yield! parseSpans (chars.Tail) chars
    | LineBreak chars ->
        yield! emitLiteral
        yield HardLineBreak
        yield! parseSpans [] chars
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

module List =
    let partitionWhile f =
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs
            | xs -> List.rev acc, xs
        loop []

let (|PrefixedLines|) prefix (lines:list<string>) =
    let prefixed, other =
        lines |> List.partitionWhile (fun line ->
            line.StartsWith(prefix))
    [ for line in prefixed ->
        line.Substring(prefix.Length)], other

let (|LineSeparated|) lines =
    let isWhite = System.String.IsNullOrWhiteSpace
    match List.partitionWhile (isWhite >> not) lines with
    | par, _::rest
    | par, ([] as rest) -> par, rest

let (|AsCharList|) (str:string) =
    List.ofSeq str

let (|Heading|_|) = function
    | StartsWith ['#'; ' '] heading ->
        Some(1, heading)
    | StartsWith ['#'; '#'; ' '] heading ->
        Some(2, heading)
    | _ -> None
        
let rec parseBlocks lines = seq {
    match lines with    
    | AsCharList(Heading (size, heading))::lines ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
    | PrefixedLines " " (body, lines) when body <> [] ->
        yield CodeBlock(body)
        yield! parseBlocks lines
    | LineSeparated (body, lines) when body <> [] ->
        let body = String.concat " " body |> List.ofSeq
        yield Paragraph(parseSpans [] body |> List.ofSeq)
        yield! parseBlocks lines   
    | line::lines when System.String.IsNullOrWhiteSpace(line) ->
        yield! parseBlocks lines
    | _ -> () }

let sample = """## Introducing F#
F# is a _functional-first_ language,
which looks like this:
let msg = "world"
printfn "hello %s!" msg
This sample prints `hello world!`
"""

let sampleDoc =
    sample.Split('\r', '\n') |> List.ofSeq
    |> parseBlocks |> List.ofSeq
   
let (PrefixedLines "..." res) = ["1"; "...2"; "...3" ]
printfn "%A" res

"[F# `home` page](http://fsharp.net) and other" |> List.ofSeq
|> parseSpans []
|> List.ofSeq

"hello \n\rworld \r!!!" |> List.ofSeq
|> parseSpans []
|> List.ofSeq

