open Core.Std

module ExpressionParseTree : sig
    type t =
        | VariableLookup of string * t
        | IndexLookup of int * t
        | EOF
end

module Command : sig
    type t =
        | ForEach of string * ExpressionParseTree.t
end

module Filter : sig
    type t =
        | Markdown of t
        | Capitalize of t
        | EOF
end

module ParseTree : sig 
    type t =
        | Text of string * t
        | Interpolation of ExpressionParseTree.t * Filter.t * t
        | Command of Command.t * t * t
        | EOF
end

val parse : Lexer.Token.t list -> ParseTree.t 
