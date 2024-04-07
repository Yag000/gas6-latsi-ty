open Alcotest
open Latsi.Interpreter
open Utils

let eval_str program =
  let lexbuf = Lexing.from_string program in
  let ast = Latsi.Parser.input Latsi.Lexer.lexer lexbuf in
  Implementation.eval_env ast

let eval_env_str program = try Some (eval_str program) with _ -> None

let test_eval name program constraints =
  let result = eval_env_str program in
  test_case name `Quick (fun () ->
      check bool "eval" true
        (match result with
        | Some env ->
            List.for_all
              (fun (n, v) -> Implementation.is_correct_value n v env)
              constraints
        | None -> false))

let test_empty_program program =
  test_case "Empty program" `Quick (fun () ->
      check_raises "Empty program" Empty_program (fun () ->
          let _ = eval_str program in
          ()))

let test_eval_fail name program =
  let result = eval_env_str program in
  test_case name `Quick (fun () -> check bool "eval" true (result = None))

let qtest_assigment_one_variable =
  let open QCheck in
  Test.make ~count:1000 ~name:"Assignment one variable" (pair arbitrary_var int)
    (fun (var, value) ->
      let program = Printf.sprintf "0 %c = %d\n" var value in
      let result = eval_env_str program in
      match result with
      | Some env -> Implementation.is_correct_value var value env
      | None -> assert false (* Impossible since the program is correct *))

let qtest_bin_op op op_char =
  let open QCheck in
  Test.make ~count:1000 ~name:(Printf.sprintf "Binary operation %c" op_char)
    (triple arbitrary_var int int) (fun (var, value1, value2) ->
      let program =
        Printf.sprintf "0 %c = %d %c %d\n" var value1 op_char value2
      in
      let result = eval_env_str program in
      match result with
      | Some env -> Implementation.is_correct_value var (op value1 value2) env
      | None -> assert false (* Impossible since the program is correct *))

let qtest_unop op op_char =
  let open QCheck in
  Test.make ~count:1000 ~name:(Printf.sprintf "Unary operation %c" op_char)
    (pair arbitrary_var int) (fun (var, value) ->
      let program = Printf.sprintf "0 %c = %c %d\n" var op_char value in
      let result = eval_env_str program in
      match result with
      | Some env -> Implementation.is_correct_value var (op value) env
      | None -> assert false (* Impossible since the program is correct *))

let test_var_assignment =
  [
    test_eval "X = 1" "0 X = 1\n" [ ('X', 1) ];
    test_eval "X = 1; Y = 2" "0 X = 1\n 10 Y = 2\n" [ ('X', 1); ('Y', 2) ];
    test_eval "X = 1; Y = X" "0 X = 1\n 10 Y = X\n" [ ('X', 1); ('Y', 1) ];
    test_eval "X = 1; Y = 10 ; X = 9" "0 X = 1\n 10 Y = 10\n 20 X = 9\n"
      [ ('X', 9); ('Y', 10) ];
    QCheck_alcotest.to_alcotest qtest_assigment_one_variable;
  ]

let test_addition =
  [
    test_eval "X = 1 + 2" "0 X = 1 + 2\n" [ ('X', 3) ];
    test_eval "X = 1 + 2 + 3" "0 X = 1 + 2 + 3\n" [ ('X', 6) ];
    test_eval "X = 1 + 2 + 3 + 4" "0 X = 1 + 2 + 3 + 4\n" [ ('X', 10) ];
    test_eval "X = 1 + 2 + 3 + 4 + 5" "0 X = 1 + 2 + 3 + 4 + 5\n" [ ('X', 15) ];
    test_eval "X = 1; Y = 2; Z = X + Y" "0 X = 1\n 10 Y = 2\n 20 Z = X + Y\n"
      [ ('X', 1); ('Y', 2); ('Z', 3) ];
    QCheck_alcotest.to_alcotest (qtest_bin_op ( + ) '+');
  ]

let test_substraction =
  [
    test_eval "X = 1 - 2" "0 X = 1 - 2\n" [ ('X', -1) ];
    test_eval "X = 1 - 2 - 3" "0 X = 1 - 2 - 3\n" [ ('X', -4) ];
    test_eval "X = 1 - 2 - 3 - 4" "0 X = 1 - 2 - 3 - 4\n" [ ('X', -8) ];
    test_eval "X = 1 - 2 - 3 - 4 - 5" "0 X = 1 - 2 - 3 - 4 - 5\n" [ ('X', -13) ];
    test_eval "X = 1; Y = 2; Z = X - Y" "0 X = 1\n 10 Y = 2\n 20 Z = X - Y\n"
      [ ('X', 1); ('Y', 2); ('Z', -1) ];
    QCheck_alcotest.to_alcotest (qtest_bin_op ( - ) '-');
  ]

let test_multiplication =
  [
    test_eval "X = 1 * 2" "0 X = 1 * 2\n" [ ('X', 2) ];
    test_eval "X = 1 * 2 * 3" "0 X = 1 * 2 * 3\n" [ ('X', 6) ];
    test_eval "X = 1 * 2 * 3 * 4" "0 X = 1 * 2 * 3 * 4\n" [ ('X', 24) ];
    test_eval "X = 1 * 2 * 3 * 4 * 5" "0 X = 1 * 2 * 3 * 4 * 5\n" [ ('X', 120) ];
    test_eval "X = 1; Y = 2; Z = X * Y" "0 X = 1\n 10 Y = 2\n 20 Z = X * Y\n"
      [ ('X', 1); ('Y', 2); ('Z', 2) ];
    QCheck_alcotest.to_alcotest (qtest_bin_op ( * ) '*');
  ]

let test_division =
  [
    test_eval "X = 1 / 2" "0 X = 1 / 2\n" [ ('X', 0) ];
    test_eval "X = 100 / 2 / 3" "0 X = 100 / 2 / 3\n" [ ('X', 16) ];
    test_eval "X = 100 / 2 / 3 / 4" "0 X = 100 / 2 / 3 / 4\n" [ ('X', 4) ];
    test_eval "X = 1; Y = 2; Z = X / Y" "0 X = 1\n 10 Y = 2\n 20 Z = X / Y\n"
      [ ('X', 1); ('Y', 2); ('Z', 0) ];
    QCheck_alcotest.to_alcotest (qtest_bin_op ( / ) '/');
  ]

let test_illegal_integer_division =
  [
    test_eval_fail "X = 1 / 0" "0 X = 1 / 0\n";
    test_eval_fail "X = 1 / Y" "0 X = 1 / Y\n";
    test_eval_fail "X = 1 / (1 - 1)" "0 X = 1 / (1 - 1)\n";
  ]

let test_unop_pos =
  [
    test_eval "X = +1" "0 X = +1\n" [ ('X', 1) ];
    test_eval "X = +1 + 2" "0 X = +1 + 2\n" [ ('X', 3) ];
    test_eval "X = +1 + 2 + 3" "0 X = +1 + 2 + 3\n" [ ('X', 6) ];
    test_eval "X = +1 + 2 + 3 + 4" "0 X = +1 + 2 + 3 + 4\n" [ ('X', 10) ];
    test_eval "X = +1 + 2 + 3 + 4 + 5" "0 X = +1 + 2 + 3 + 4 + 5\n"
      [ ('X', 15) ];
    test_eval "X = +1; Y = 2; Z = X + Y" "0 X = +1\n 10 Y = 2\n 20 Z = X + Y\n"
      [ ('X', 1); ('Y', 2); ('Z', 3) ];
    QCheck_alcotest.to_alcotest (qtest_unop (fun x -> x) '+');
  ]

let test_unop_neg =
  [
    test_eval "X = -1" "0 X = -1\n" [ ('X', -1) ];
    test_eval "X = -1 + 2" "0 X = -1 + 2\n" [ ('X', 1) ];
    test_eval "X = -1 + 2 + 3" "0 X = -1 + 2 + 3\n" [ ('X', 4) ];
    test_eval "X = -1 + 2 + 3 + 4" "0 X = -1 + 2 + 3 + 4\n" [ ('X', 8) ];
    QCheck_alcotest.to_alcotest (qtest_unop (fun x -> -x) '-');
  ]

let test_parenthesis_unops =
  [
    test_eval "X = 1 + 2" "0 X = 1 + 2\n" [ ('X', 3) ];
    test_eval "X = -(1 + 2)" "0 X = -(1 + 2)\n" [ ('X', -3) ];
    test_eval "X = -(1 - 2)" "0 X = -(1 - 2)\n" [ ('X', 1) ];
    test_eval "X = +(-1 + 2)" "0 X = +(-1 + 2)\n" [ ('X', 1) ];
    test_eval "X = +(-1 + -2)" "0 X = +(-1 + -2)\n" [ ('X', -3) ];
  ]

let test_arithmetic_operations =
  [
    test_eval "X = 1 + 3 - 1" "0 X = 1 + 3 - 1\n" [ ('X', 3) ];
    test_eval "X = 1 + 3 - 1 * 2" "0 X = 1 + 3 - 1 * 2\n" [ ('X', 2) ];
    test_eval "X = 1 + 3 - 1 * 2 / 2" "0 X = 1 + 3 - 1 * 2 / 2\n" [ ('X', 3) ];
    test_eval "X = (1 - 10) * 7 / (3 * 2) + 1"
      "0 X = (1 - 10) * 7 / (3 * 2) + 1\n"
      [ ('X', -9) ];
  ]

let empty_constraints =
  List.init 26 (fun i -> (char_of_int (i + int_of_char 'A'), 0))

let test_remarks =
  [
    test_eval "REM \"Hello, World\"" "0 REM \"Hello, World\"\n"
      empty_constraints;
  ]

let test_line_order =
  [
    test_eval "0 X = 1; 0 X = 2" "0 X = 1\n 0 X = 2\n" [ ('X', 2) ];
    test_eval "0 Y = 1; 0 X = 2" "0 Y = 1\n 0 X = 2\n" [ ('X', 2); ('Y', 0) ];
    test_eval "0 X = 1; 10 Y = 8 ;0 X = 2" "0 X = 1\n 10 Y = 8 \n 0 X = 2\n"
      [ ('X', 2); ('Y', 8) ];
    test_eval "10 X = 1; 0 Y = 1; 6 X = Y + 2"
      "10 X = 1\n 0 Y = 1\n 6 X = Y + 2\n"
      [ ('X', 1); ('Y', 1) ];
    test_eval "10 X = 1; 0 Y = 1; 16 X = Y + 2; 0 Y = 2"
      "10 X = 1\n 0 Y = 1\n 16 X = Y + 2\n 0 Y = 2\n"
      [ ('X', 4); ('Y', 2) ];
  ]

let () =
  run "Interpreter"
    [
      ("Empty program", [ test_empty_program "" ]);
      ("Variable assignment", test_var_assignment);
      ("Addition", test_addition);
      ("Substraction", test_substraction);
      ("Multiplication", test_multiplication);
      ("Division", test_division);
      ("Illegal integer division", test_illegal_integer_division);
      ("Unop Pos", test_unop_pos);
      ("Unop Neg", test_unop_neg);
      ("Parenthesis unops", test_parenthesis_unops);
      ("Atithmetic operations", test_arithmetic_operations);
      ("Remarks", test_remarks);
      ("Line execution order", test_line_order);
    ]
