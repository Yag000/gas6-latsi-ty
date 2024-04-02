open Latsi.Ast
open Utils
open Alcotest

let parse s =
  try
    let lexbuf = Lexing.from_string s in
    Some (Latsi.Parser.input Latsi.Lexer.lexer lexbuf)
  with _ -> None

let program_test_case name s expected =
  let actual = parse s in
  test_case name `Quick (fun () ->
      check (option program_testable) "same program" expected actual)

let option_instr_test_case name s expected =
  let program =
    Option.map (fun expected -> [ { number = 0; instr = expected } ]) expected
  in
  program_test_case name ("0 " ^ s ^ "\n") program

let fail_instr_test_case name s = option_instr_test_case name s None

let instr_test_case name s expected =
  option_instr_test_case name s (Some expected)

let () =
  run "Parser"
    [
      ( "Assign",
        [
          instr_test_case "simple integer" "X = 1" (Assign ('X', Number 1));
          fail_instr_test_case "invalid variable" "x = 1";
          fail_instr_test_case "assigning a string" "X = \"1\"";
          instr_test_case "X = Y" "X = Y" (Assign ('X', Var 'Y'));
          instr_test_case "X = X" "X = X" (Assign ('X', Var 'X'));
        ] );
      ( "Unary operations",
        [
          instr_test_case "positive integer alone" "X = +1"
            (Assign ('X', Unop (Pos, Number 1)));
          instr_test_case "negative integer alone" "X = -1"
            (Assign ('X', Unop (Neg, Number 1)));
        ] );
      ( "Sum",
        [
          instr_test_case "simple integer" "X = 1" (Assign ('X', Number 1));
          instr_test_case "assign with expression" "X = 1 + 2"
            (Assign ('X', Binop (Add, Number 1, Number 2)));
          instr_test_case "assign with expression" "X = 1 + 2 + 3"
            (Assign ('X', Binop (Add, Binop (Add, Number 1, Number 2), Number 3)));
        ] );
      ( "Sub",
        [
          instr_test_case "assign with expression" "X = 1 - 2"
            (Assign ('X', Binop (Sub, Number 1, Number 2)));
          instr_test_case "assign with expression" "X = 1 - 2 - 3"
            (Assign ('X', Binop (Sub, Binop (Sub, Number 1, Number 2), Number 3)));
        ] );
      ( "Mul",
        [
          instr_test_case "assign with expression" "X = 1 * 2"
            (Assign ('X', Binop (Mul, Number 1, Number 2)));
          instr_test_case "assign with expression" "X = 1 * 2 * 3"
            (Assign ('X', Binop (Mul, Binop (Mul, Number 1, Number 2), Number 3)));
        ] );
      ( "Div",
        [
          instr_test_case "assign with expression" "X = 1 / 2"
            (Assign ('X', Binop (Div, Number 1, Number 2)));
          instr_test_case "assign with expression" "X = 1 / 2 / 3"
            (Assign ('X', Binop (Div, Binop (Div, Number 1, Number 2), Number 3)));
        ] );
      ( "Integer operations precedence",
        [
          instr_test_case "assign with expression" "X = 1 + 2 * 3"
            (Assign ('X', Binop (Add, Number 1, Binop (Mul, Number 2, Number 3))));
          instr_test_case "assign with expression" "X = 1 * 2 + 3"
            (Assign ('X', Binop (Add, Binop (Mul, Number 1, Number 2), Number 3)));
          instr_test_case "assign with expression" "X = 1 * 2 + 3 * 4"
            (Assign
               ( 'X',
                 Binop
                   ( Add,
                     Binop (Mul, Number 1, Number 2),
                     Binop (Mul, Number 3, Number 4) ) ));
          instr_test_case "assign with expression" "X = 1 + 2 * 3 + 4"
            (Assign
               ( 'X',
                 Binop
                   ( Add,
                     Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                     Number 4 ) ));
        ] );
      ( "Parenthesis in binary operations",
        [
          instr_test_case "assign with expression" "X = (1 + 2) * 3"
            (Assign ('X', Binop (Mul, Binop (Add, Number 1, Number 2), Number 3)));
          instr_test_case "assign with expression" "X = 1 * (2 + 3)"
            (Assign ('X', Binop (Mul, Number 1, Binop (Add, Number 2, Number 3))));
          instr_test_case "assign with expression" "X = (1 + 2) * (3 + 4)"
            (Assign
               ( 'X',
                 Binop
                   ( Mul,
                     Binop (Add, Number 1, Number 2),
                     Binop (Add, Number 3, Number 4) ) ));
          instr_test_case "assign with expression" "X = (1 + 2) * 3 + 4"
            (Assign
               ( 'X',
                 Binop
                   ( Add,
                     Binop (Mul, Binop (Add, Number 1, Number 2), Number 3),
                     Number 4 ) ));
          instr_test_case "assign with expression" "X = 1 + (2 * 3) + 4"
            (Assign
               ( 'X',
                 Binop
                   ( Add,
                     Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                     Number 4 ) ));
        ] );
      ( "Parenthesis in unary operations",
        [
          instr_test_case "assign with expression" "X = -(1 + 2)"
            (Assign ('X', Unop (Neg, Binop (Add, Number 1, Number 2))));
          instr_test_case "assign with expression" "X = +(1 + 2)"
            (Assign ('X', Unop (Pos, Binop (Add, Number 1, Number 2))));
        ] );
      ( "REM",
        [
          instr_test_case "assign with expression" "REM \"yes\"" (Rem "yes");
          fail_instr_test_case "assign with expression" "REM \"yes\" \"no\"";
          fail_instr_test_case "assign with expression" "REM yes";
          fail_instr_test_case "assign with expression" "REM 1";
        ] );
      ( "Line",
        [
          program_test_case "non CR terminated line" "0 X = 1" None;
          program_test_case "CR terminated line" "0 X = 1 \n"
            (Some [ { number = 0; instr = Assign ('X', Number 1) } ]);
          program_test_case "Nultiple lines" "0 X = 1\n 10 Y = 2\n"
            (Some
               [
                 { number = 0; instr = Assign ('X', Number 1) };
                 { number = 10; instr = Assign ('Y', Number 2) };
               ]);
        ] );
      ("Program", [ program_test_case "empty program" "" (Some []) ]);
    ]
