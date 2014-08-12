open Core.Std;;
open OUnit2;;
open Lexer;;
open Parser;;
open Evaluator;;

let id item = item

let basic_evaluating _ =
    let input = "Hoojama" in
    let parsed = parse (lex input) in
    assert_equal "Hoojama" (evaluate parsed `Null)

let basic_interpolation _ =
    let json = Yojson.Basic.from_string "{\"user\":\"Llama\"}" in
    let input = "Hi {{user}}" in
    let parsed = parse (lex input) in
    assert_equal "Hi Llama" (evaluate parsed json) ?printer: (Some id)

let deep_interpolation _ =
    let json = Yojson.Basic.from_string "{
        \"user\": {
            \"first_name\": \"Llama\",
            \"last_name\": \"Drama\"
        }
    }" in
    let input = "Hi {{user.last_name}}" in
    let parsed = parse (lex input) in
    assert_equal "Hi Drama" (evaluate parsed json) ?printer: (Some id)

let list_lookup _ =
    let json = Yojson.Basic.from_string "{
        \"users\": [
            \"Llama\",
            \"Drama\"
        ]
    }" in
    let input = "Hi {{users[1]}}" in
    let parsed = parse (lex input) in
    assert_equal "Hi Drama" (evaluate parsed json) ?printer: (Some id)

let markdown _ =
    let json = Yojson.Basic.from_string "{\"user\":\"Llama\"}" in
    let input = "Hi {{user|markdown}}" in
    let parsed = parse (lex input) in
    assert_equal "Hi <p>Llama</p>\n" (evaluate parsed json) ?printer: (Some id)

let foreach _ =
    let json = Yojson.Basic.from_string "{
        \"users\": [
            \"Llama\",
            \"Drama\"
        ]
    }" in
    let input = "{% for user in users %}{{user}}{% endfor %}" in
    let parsed = parse (lex input) in
    assert_equal "LlamaDrama" (evaluate parsed json) ?printer: (Some id)

let tests = [
    "basic_evaluating" >:: basic_evaluating;
    "basic_interpolation" >:: basic_interpolation;
    "deep_interpolation" >:: deep_interpolation;
    "list_lookup" >:: list_lookup;
    "markdown" >:: markdown;
    "foreach" >:: foreach
]
