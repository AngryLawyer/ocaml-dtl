open Core.Std;;
open OUnit2;;
open Lexer;;
open Parser;;

let basic_parsing _ =
    assert_equal (ParseTree.Text("Cat", ParseTree.EOF)) (parse [Token.String("Cat")]);
    assert_equal (ParseTree.Text("I love ", ParseTree.Interpolation(ExpressionParseTree.VariableLookup("cat", ExpressionParseTree.EOF), Filter.EOF, ParseTree.EOF))) (parse [Token.String("I love "); Token.OpenBrace; Token.String("cat"); Token.CloseBrace]);
    assert_equal (ParseTree.Text("I love ", ParseTree.Interpolation(ExpressionParseTree.VariableLookup("cat", ExpressionParseTree.VariableLookup("name", ExpressionParseTree.EOF)), Filter.EOF, ParseTree.EOF))) (parse [Token.String("I love "); Token.OpenBrace; Token.String("cat.name"); Token.CloseBrace]);

    assert_equal (ParseTree.Text("I love ",
        ParseTree.Interpolation(
            ExpressionParseTree.VariableLookup("cat",
                ExpressionParseTree.IndexLookup(7,
                    ExpressionParseTree.EOF)),
            Filter.EOF,
            ParseTree.EOF)
     )) (parse [Token.String("I love "); Token.OpenBrace; Token.String("cat[7]"); Token.CloseBrace])

let filter_parsing _ =
    assert_equal (ParseTree.Text("I love ",
        ParseTree.Interpolation(
            ExpressionParseTree.VariableLookup("cat", ExpressionParseTree.EOF),
            Filter.Markdown(Filter.EOF),
            ParseTree.EOF)
     )) (parse [Token.String("I love "); Token.OpenBrace; Token.String("cat|shave"); Token.CloseBrace])

let foreach_parsing _ =
    let tokens = [Token.OpenCommand; Token.String("for item in items"); Token.CloseCommand; Token.String("<p>LOL</p>"); Token.OpenCommand; Token.String("endfor"); Token.CloseCommand] in
    let expected_parsing =
        ParseTree.Command(
            Command.ForEach(
                "item",
                ExpressionParseTree.VariableLookup("items", ExpressionParseTree.EOF)
            ),
            ParseTree.Text("<p>LOL</p>", ParseTree.EOF),
            ParseTree.EOF
        ) in
    assert_equal expected_parsing (parse tokens)

let tests = [
    "basic_parsing" >:: basic_parsing;
    "filter_parsing" >:: filter_parsing;
    "foreach_parsing" >:: foreach_parsing
]
