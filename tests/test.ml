open Core.Std;;
open OUnit2;;

let suite =
    "CamlMapper" >::: List.concat [
        Test_lexer.tests;
        Test_parser.tests;
        Test_evaluator.tests
    ]
;;

let () =
    run_test_tt_main suite
;;
