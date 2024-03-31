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
      check (option program_testable) "same program" (Some expected) actual)

let instr_test_case name s expected =
  let program = [ { number = 0; instr = expected } ] in
  program_test_case name ("0 " ^ s ^ "\n") program

let () =
  run "Parser"
    [
      ( "sum",
        [
          instr_test_case "simple integer" "X = 1" (Assign ('X', Number 1));
          instr_test_case "assign with expression" "X = 1 + 2"
            (Assign ('X', Binop (Add, Number 1, Number 2)));
          instr_test_case "assign with expression" "X = 1 + 2 + 3"
            (Assign ('X', Binop (Add, Binop (Add, Number 1, Number 2), Number 3)));
        ] );
    ]
