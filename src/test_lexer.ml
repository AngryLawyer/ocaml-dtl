open Core.Std;;
open OUnit2;;
open Lexer;;

(* Ultra-simplistic Lexing tests *)
let basic_lexing _ =
    assert_equal [Token.String("Hoojama")] (lex "Hoojama");
    assert_equal [Token.OpenBrace] (lex "{{");
    assert_equal [Token.OpenCommand; Token.String("Hoojama"); Token.CloseCommand] (lex "{%Hoojama%}");
    assert_equal [Token.OpenCommand; Token.String("Hoojama"); Token.CloseCommand; Token.String(" llama")] (lex "{%Hoojama%} llama")

let basic_expression_lexing _ =
    assert_equal [ExpressionToken.String("Cat")] (lex_expression "Cat");
    assert_equal [ExpressionToken.String("Cat"); ExpressionToken.Period; ExpressionToken.String("whisker")] (lex_expression "Cat.whisker");
    assert_equal [ExpressionToken.String("Cat"); ExpressionToken.OpenSquareBracket; ExpressionToken.String("0"); ExpressionToken.CloseSquareBracket] (lex_expression "Cat[0]");
    assert_equal [ExpressionToken.String("Cat"); ExpressionToken.OpenSquareBracket; ExpressionToken.String("0"); ExpressionToken.CloseSquareBracket; ExpressionToken.OpenSquareBracket; ExpressionToken.String("0"); ExpressionToken.CloseSquareBracket] (lex_expression "Cat[0][0]");
    assert_equal [ExpressionToken.String("Cat"); ExpressionToken.OpenSquareBracket; ExpressionToken.String("0"); ExpressionToken.CloseSquareBracket; ExpressionToken.Pipe; ExpressionToken.String("cuddle")] (lex_expression "Cat[0]|cuddle")

let tests = [
    "basic_lexing" >:: basic_lexing;
    "basic_expression_lexing" >:: basic_expression_lexing
]
