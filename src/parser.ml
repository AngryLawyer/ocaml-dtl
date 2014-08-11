open Core.Std

module ExpressionParseTree = struct
    type t =
        | VariableLookup of string * t
        | IndexLookup of int * t
        | EOF
end

module Command = struct
    type t =
        | ForEach of string * ExpressionParseTree.t
end

module Filter = struct
    type t =
        | Markdown of t
        | Capitalize of t
        | EOF
end

module ParseTree = struct
    type t =
        | Text of string * t
        | Interpolation of ExpressionParseTree.t * Filter.t * t
        | Command of Command.t * t * t
        | EOF
end

let get_closer_text command =
    match command with
    | Command.ForEach (_) -> "endfor"


let rec parse_expression tokens =
    match tokens with
    | Lexer.ExpressionToken.String(text) :: tail ->
            ExpressionParseTree.VariableLookup (text, parse_expression_after_string tail)
    | _ ->
            raise (Failure "Unexpected token")
    and parse_expression_after_string tokens =
        match tokens with
        | Lexer.ExpressionToken.Period :: Lexer.ExpressionToken.String(text) :: tail -> 
                ExpressionParseTree.VariableLookup (text, parse_expression_after_string tail)
        | Lexer.ExpressionToken.OpenSquareBracket :: Lexer.ExpressionToken.String(text) :: Lexer.ExpressionToken.CloseSquareBracket :: tail ->
                ExpressionParseTree.IndexLookup(Int.of_string(text), parse_expression_after_string tail)
        | Lexer.ExpressionToken.Pipe :: _ ->
                ExpressionParseTree.EOF
        | [] ->
                ExpressionParseTree.EOF
        | _ ->
                raise (Failure "Unexpected token")

let rec parse_filter_wind_forward tokens =
    match tokens with
    | [] -> Filter.EOF
    | Lexer.ExpressionToken.Pipe :: Lexer.ExpressionToken.String(text) :: tail ->
            parse_filter_type text tail
    | _ :: tail ->
            parse_filter_wind_forward tail
and parse_filter tokens =
    match tokens with
    | [] -> Filter.EOF
    | Lexer.ExpressionToken.Pipe :: Lexer.ExpressionToken.String(text) :: tail ->
            parse_filter_type text tail
    | _ ->
            raise (Failure "Unexpected token in filter")
and parse_filter_type text tail = 
    match text with
    | "markdown" -> Filter.Markdown(parse_filter tail)
    | "capitalize" -> Filter.Capitalize(parse_filter tail)
    | _ -> raise (Failure (sprintf "Unknown filter %s" text))

let generate_command text =
    let text = String.strip text in
    (* FIXME: The following doesn't match on square brackets *)
    let regex = Re_str.regexp "for[ ]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ ]+in[ ]+\\([A-Za-z_][A-Za-z0-9_.]*\\)*" in
    if Re_str.string_match regex text 0 then
        let key = Re_str.matched_group 1 text in
        let iterated = Re_str.matched_group 2 text in
        let lexed = Lexer.lex_expression iterated in
        let parsed_expression = parse_expression lexed in
        Command.ForEach(key, parsed_expression)
    else
        raise (Failure "Bad Foreach")


let rec parse tokens =
    match tokens with
    | [] -> ParseTree.EOF
    | Lexer.Token.String (item) :: tail -> ParseTree.Text (item, parse tail)
    | Lexer.Token.OpenBrace :: Lexer.Token.String(text) :: Lexer.Token.CloseBrace :: tail ->
        let lexed = Lexer.lex_expression text in
        let parsed_expression = parse_expression lexed in
        let parsed_filter = parse_filter_wind_forward lexed in
        ParseTree.Interpolation(parsed_expression, parsed_filter, parse tail)
    | Lexer.Token.OpenCommand :: Lexer.Token.String(text) :: Lexer.Token.CloseCommand :: tail ->
        let command = generate_command text in
        let closer_text = (get_closer_text command) in
        let (tokens, rest) = collate_tokens tail closer_text [] in
        ParseTree.Command(command, parse tokens, parse rest)
    | _ -> raise (Failure "Unexpected token")

    and collate_tokens tokens closer_text collated =
        match tokens with
        | [] -> raise (Failure "Unexpected EOF")
        | (Lexer.Token.OpenCommand as a) :: (Lexer.Token.String(text) as b) :: (Lexer.Token.CloseCommand as c) :: tail ->
            if ((String.strip text) = closer_text) then  
                (List.rev collated), tail
            else
                collate_tokens tail closer_text (c :: b :: a :: collated)
        | head :: tail -> collate_tokens tail closer_text (head :: collated)
