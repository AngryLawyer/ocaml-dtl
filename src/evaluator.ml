(* #require "omd" *)
open Core.Std

let apply_markdown text =
    Omd.of_string text |> Omd.to_html

let apply_capitalize text =
    String.capitalize text

let rec apply_filters text filters =
    match filters with
    | Parser.Filter.EOF -> text
    | Parser.Filter.Capitalize(next) ->
        apply_filters (apply_capitalize text) next
    | Parser.Filter.Markdown(next) ->
        apply_filters (apply_markdown text) next


let rec walk_expression_tree expression_tree json =
    let open Yojson.Basic.Util in
    match expression_tree with
        | Parser.ExpressionParseTree.EOF ->
                json
        | Parser.ExpressionParseTree.VariableLookup (text, next) ->
                let json = json |> member text in
                walk_expression_tree next json
        | Parser.ExpressionParseTree.IndexLookup (position, next) ->
                let json = json |> index position in
                walk_expression_tree next json


let interpolate expression_tree json =
    let open Yojson.Basic.Util in
    (walk_expression_tree expression_tree json) |> to_string_option

let rec evaluate_for_each variable lookup json contents =
    let open Yojson.Basic.Util in
    let unpacked = begin match json with
    | `Assoc (unpacked) -> unpacked
    | _ -> [] end in
    let items = walk_expression_tree lookup json in
    let repeated = List.map (to_list items) ~f: fun new_root -> (
        let json = `Assoc ((variable, new_root) :: unpacked) in
        evaluate contents json
    ) in
    String.concat repeated

and evaluate_inner parse_tree json strings =
    match parse_tree with
    | Parser.ParseTree.Text (text, next) ->
            evaluate_inner next json ((sprintf "%s" text) :: strings)
    | Parser.ParseTree.Interpolation (interpolation, filter, next) ->
            begin
            match interpolate interpolation json with
            | Some interpolated_text -> 
                    let filtered = apply_filters interpolated_text filter in
                    evaluate_inner next json ((sprintf "%s" filtered) :: strings)
            | None ->
                    evaluate_inner next json strings
            end
    | Parser.ParseTree.Command (command, contents, next) ->
            begin
            match command with
            | Parser.Command.ForEach (variable, lookup) ->
                evaluate_inner next json ((evaluate_for_each variable lookup json contents) :: strings)
            end
    | Parser.ParseTree.EOF -> List.rev strings

and evaluate parse_tree json =
    String.concat (evaluate_inner parse_tree json [])
