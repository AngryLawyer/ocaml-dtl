open Core.Std

module Token = struct
    type t =
    | OpenBrace
    | CloseBrace
    | OpenCommand
    | CloseCommand
    | String of string
end

module ExpressionToken = struct 
    type t =
    | OpenSquareBracket
    | CloseSquareBracket
    | Period
    | Pipe
    | Space
    | String of string
end

let explode a_string =
    let chars = String.to_list a_string in
    List.map chars ~f:(fun item -> Char.to_string item)

let collect accumulator =
    (String.concat (List.rev accumulator))

let collect_text accumulator tokens =
    match accumulator with
    | [] -> tokens
    | text -> Token.String (collect accumulator) :: tokens

let collect_expression_text accumulator tokens =
    match accumulator with
    | [] -> tokens
    | text -> ExpressionToken.String (collect accumulator) :: tokens

let rec lex_input input accumulator tokens =
    match input with
        | [] -> collect_text accumulator tokens
        | "{" :: "{" :: tail -> lex_input tail [] (Token.OpenBrace :: (collect_text accumulator tokens))
        | "{" :: "%" :: tail -> lex_input tail [] (Token.OpenCommand :: (collect_text accumulator tokens))
        | "}" :: "}" :: tail -> lex_input tail [] (Token.CloseBrace :: (collect_text accumulator tokens))
        | "%" :: "}" :: tail -> lex_input tail [] (Token.CloseCommand :: (collect_text accumulator tokens))
        | head :: tail -> lex_input tail (head :: accumulator) tokens

let lex input =
    let exploded = explode input in
    List.rev (lex_input exploded [] [])

let rec lex_expression_input input accumulator tokens =
    match input with
        | [] -> collect_expression_text accumulator tokens
        | (" " :: tail) | ("\n" :: tail) -> lex_expression_input tail [] (ExpressionToken.Space :: (collect_expression_text accumulator tokens))
        | "." :: tail -> lex_expression_input tail [] (ExpressionToken.Period :: (collect_expression_text accumulator tokens))
        | "[" :: tail -> lex_expression_input tail [] (ExpressionToken.OpenSquareBracket :: (collect_expression_text accumulator tokens))
        | "]" :: tail -> lex_expression_input tail [] (ExpressionToken.CloseSquareBracket :: (collect_expression_text accumulator tokens))
        | "|" :: tail -> lex_expression_input tail [] (ExpressionToken.Pipe :: (collect_expression_text accumulator tokens))
        | head :: tail -> lex_expression_input tail (head :: accumulator) tokens

let lex_expression input =
    let exploded = explode input in
    List.rev (lex_expression_input exploded [] [])
