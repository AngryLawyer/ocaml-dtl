open Core.Std

module Token : sig
    type t =
    | OpenBrace
    | CloseBrace
    | OpenCommand
    | CloseCommand
    | String of string
end

val lex : string -> Token.t list

module ExpressionToken : sig
    type t =
    | OpenSquareBracket
    | CloseSquareBracket
    | Period
    | Pipe
    | Space
    | String of string
end

val lex_expression : string -> ExpressionToken.t list
