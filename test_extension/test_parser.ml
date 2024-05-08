open Latsi_extension.Ast
open Utils
open Alcotest

let parse s =
  try
    let lexbuf = Lexing.from_string s in
    Some (Latsi_extension.Parser.input Latsi_extension.Lexer.lexer lexbuf)
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

let parse_correct_solo_assign_qcheck =
  let open QCheck in
  Test.make ~count:500 ~name:"parse correct solo assign"
    (pair arbitrary_var (int_range 0 1000))
    (fun (v, i) ->
      let s = Printf.sprintf "0 %c = %d\n" v i in
      let expected =
        Some [ { number = 0; instr = Assign [ (v, Number i) ] } ]
      in
      let actual = parse s in
      expected = actual)

let parse_correct_multi_assign_qcheck =
  let open QCheck in
  Test.make ~count:1000 ~name:"parse correct multi assign"
    (list (pair arbitrary_var (int_range 0 1000)))
    (fun l ->
      assume (List.length l > 0);
      let s =
        String.concat ", "
          (List.map (fun (v, i) -> Printf.sprintf "%c = %d" v i) l)
      in
      let s = Printf.sprintf "0 %s\n" s in
      let expected_list = List.map (fun (v, i) -> (v, Number i)) l in
      let expected = Some [ { number = 0; instr = Assign expected_list } ] in
      let actual = parse s in
      expected = actual)

let parse_correct_split_assign_qcheck =
  let open QCheck in
  Test.make ~count:1000 ~name:"parse correct split assign"
    (list (pair arbitrary_var (int_range 0 1000)))
    (fun l ->
      assume (List.length l > 1);
      let vl, il = List.split l in
      let vl_s =
        String.concat ", " (List.map (fun v -> Printf.sprintf "%c" v) vl)
      in
      let il_s =
        String.concat ", " (List.map (fun i -> Printf.sprintf "%d" i) il)
      in
      let s = Printf.sprintf "0 %s = %s\n" vl_s il_s in
      let expected =
        Some
          [
            {
              number = 0;
              instr = SplitAssign (vl, List.map (fun i -> Number i) il);
            };
          ]
      in
      let actual = parse s in
      expected = actual)

let parse_incorrect_solo_var_split_assign_qcheck =
  let open QCheck in
  Test.make ~count:500
    ~name:
      "Forall assign-type line with only 1 Var and n expressions, with n <> 1, \
       parsing fails."
    (pair arbitrary_var (list (int_range 0 1000)))
    (fun (var, int_list) ->
      assume (List.length int_list <> 1);
      let number_list =
        String.concat ", " (List.map (fun i -> Printf.sprintf "%d" i) int_list)
      in
      let s = Printf.sprintf "0 %c = %s\n" var number_list in
      Option.is_none (parse s))

let parse_incorrect_solo_expression_split_assign_qcheck =
  let open QCheck in
  Test.make ~count:500
    ~name:
      "Forall assign-type line with n Vars and only 1 expression, with n <> 1, \
       parsing fails."
    (pair (list arbitrary_var) (int_range 0 1000))
    (fun (var_list, i) ->
      assume (List.length var_list <> 1);
      let var_list =
        String.concat ", " (List.map (fun v -> Printf.sprintf "%c" v) var_list)
      in
      let s = Printf.sprintf "0 %s = %d\n" var_list i in
      Option.is_none (parse s))

let parse_incorrect_no_var_split_assign_qcheck =
  let open QCheck in
  Test.make ~count:500
    ~name:
      "Forall assign-type line with no Var and n expressions, parsing fails."
    (list (int_range 0 1000))
    (fun int_list ->
      let number_list =
        String.concat ", " (List.map (fun i -> Printf.sprintf "%d" i) int_list)
      in
      let s = Printf.sprintf "0 = %s\n" number_list in
      Option.is_none (parse s))

let parse_incorrect_no_expression_split_assign_qcheck =
  let open QCheck in
  Test.make ~count:500
    ~name:
      "Forall assign-type line with n Vars and no expression, parsing fails."
    (list arbitrary_var) (fun var_list ->
      let var_list =
        String.concat ", " (List.map (fun v -> Printf.sprintf "%c" v) var_list)
      in
      let s = Printf.sprintf "0 %s =\n" var_list in
      Option.is_none (parse s))

let () =
  run "Parser"
    [
      ( "Assign",
        [
          fail_instr_test_case "invalid variable" "x = 1";
          fail_instr_test_case "incomplete assign 1" "=";
          fail_instr_test_case "incomplete assign 2" "x =";
          fail_instr_test_case "incomplete assign 3" "= 1";
          fail_instr_test_case "assigning a string" "X = \"1\"";
          QCheck_alcotest.to_alcotest parse_correct_solo_assign_qcheck;
          instr_test_case "simple integer" "X = 1" (Assign [ ('X', Number 1) ]);
          instr_test_case "X = Y" "X = Y" (Assign [ ('X', Var 'Y') ]);
          instr_test_case "X = X" "X = X" (Assign [ ('X', Var 'X') ]);
        ] );
      ( "Multiple Assign",
        [
          instr_test_case "2 integers" "X = 1, Y = 2"
            (Assign [ ('X', Number 1); ('Y', Number 2) ]);
          instr_test_case "3 integers" "X = 1, Y = 2, Z = 3"
            (Assign [ ('X', Number 1); ('Y', Number 2); ('Z', Number 3) ]);
          instr_test_case "4 integers" "A = 1, B = 2, C = 3, D = 4"
            (Assign
               [
                 ('A', Number 1);
                 ('B', Number 2);
                 ('C', Number 3);
                 ('D', Number 4);
               ]);
          instr_test_case "26 integers"
            "A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7, H = 8, I = 9, J \
             = 10, K = 11, L = 12, M = 13, N = 14, O = 15, P = 16, Q = 17, R = \
             18, S = 19, T = 20, U = 21, V = 22, W = 23, X = 24, Y = 25, Z = \
             26"
            (Assign
               [
                 ('A', Number 1);
                 ('B', Number 2);
                 ('C', Number 3);
                 ('D', Number 4);
                 ('E', Number 5);
                 ('F', Number 6);
                 ('G', Number 7);
                 ('H', Number 8);
                 ('I', Number 9);
                 ('J', Number 10);
                 ('K', Number 11);
                 ('L', Number 12);
                 ('M', Number 13);
                 ('N', Number 14);
                 ('O', Number 15);
                 ('P', Number 16);
                 ('Q', Number 17);
                 ('R', Number 18);
                 ('S', Number 19);
                 ('T', Number 20);
                 ('U', Number 21);
                 ('V', Number 22);
                 ('W', Number 23);
                 ('X', Number 24);
                 ('Y', Number 25);
                 ('Z', Number 26);
               ]);
          instr_test_case "Same variable appears" "X = 1, X = 2, Y = 3"
            (Assign [ ('X', Number 1); ('X', Number 2); ('Y', Number 3) ]);
          fail_instr_test_case "Missing 1st Var" "= 1, Y = 2";
          fail_instr_test_case "Missing 1st relop" "X 1, Y = 2";
          fail_instr_test_case "Missing 1st expression" "X =, Y = 2";
          fail_instr_test_case "Missing comma separator" "X = 1 Y = 2";
          fail_instr_test_case "Missing 2nd Var" "X = 1, = 2";
          fail_instr_test_case "Missing 2nd relop" "X = 1, Y 2";
          fail_instr_test_case "Missing 2nd expression" "X = 1, Y = ";
          fail_instr_test_case "one invalid variable" "x = 1, Y = 2";
          fail_instr_test_case "one assigning a string" "X = \"1\", Y = 2";
          fail_instr_test_case "invalid variables" "x = 1, y = 2";
          fail_instr_test_case "assigning strings" "X = \"1\", Y = \"2\"";
          instr_test_case "X = Y, Y = X" "X = Y, Y = X"
            (Assign [ ('X', Var 'Y'); ('Y', Var 'X') ]);
          instr_test_case "X = X, Y = Y" "X = X, Y = Y"
            (Assign [ ('X', Var 'X'); ('Y', Var 'Y') ]);
          QCheck_alcotest.to_alcotest parse_correct_multi_assign_qcheck;
        ] );
      ( "Split_Assign",
        [
          QCheck_alcotest.to_alcotest parse_correct_split_assign_qcheck;
          QCheck_alcotest.to_alcotest
            parse_incorrect_solo_var_split_assign_qcheck;
          QCheck_alcotest.to_alcotest
            parse_incorrect_solo_expression_split_assign_qcheck;
          QCheck_alcotest.to_alcotest parse_incorrect_no_var_split_assign_qcheck;
          QCheck_alcotest.to_alcotest
            parse_incorrect_no_expression_split_assign_qcheck;
          instr_test_case "Swap 2 variables" "X, Y = Y, X"
            (SplitAssign ([ 'X'; 'Y' ], [ Var 'Y'; Var 'X' ]));
          instr_test_case "2 expressions" "X, Y = 1, 2 + 2"
            (SplitAssign
               ([ 'X'; 'Y' ], [ Number 1; Binop (Add, Number 2, Number 2) ]));
          instr_test_case "3 expressions" "X, Y, Z = 1, 2 + X, 3"
            (SplitAssign
               ( [ 'X'; 'Y'; 'Z' ],
                 [ Number 1; Binop (Add, Number 2, Var 'X'); Number 3 ] ));
          instr_test_case "Same variable appears" "X, X, Y = 1, 2, 3"
            (SplitAssign ([ 'X'; 'X'; 'Y' ], [ Number 1; Number 2; Number 3 ]));
          fail_instr_test_case "Missing 1st Var" ", Y = 1, 2";
          fail_instr_test_case "Missing relop" "X, Y 1, 2";
          fail_instr_test_case "Missing 1st expression" "X, Y =, 2";
          fail_instr_test_case "Missing comma separator" "X Y = 1 2";
          fail_instr_test_case "Missing 2nd Var" "X, = 1, 2";
          fail_instr_test_case "Missing 2nd expression" "X, Y = 1, ";
          fail_instr_test_case "One invalid variable" "x, Y = 1, 2";
          fail_instr_test_case "One assigning a string" "X, Y = \"1\", 2";
          fail_instr_test_case "Invalid variables" "x, y = 1, 2";
          fail_instr_test_case "Assigning strings" "X, Y = \"1\", \"2\"";
          instr_test_case "X, Y = Y, X" "X, Y = Y, X"
            (SplitAssign ([ 'X'; 'Y' ], [ Var 'Y'; Var 'X' ]));
          instr_test_case "X, Y = X, Y" "X, Y = X, Y"
            (SplitAssign ([ 'X'; 'Y' ], [ Var 'X'; Var 'Y' ]));
        ] );
      ( "Unary operations",
        [
          instr_test_case "positive integer alone" "X = +1"
            (Assign [ ('X', Unop (Pos, Number 1)) ]);
          instr_test_case "negative integer alone" "X = -1"
            (Assign [ ('X', Unop (Neg, Number 1)) ]);
        ] );
      ( "Sum",
        [
          instr_test_case "simple integer" "X = 1" (Assign [ ('X', Number 1) ]);
          instr_test_case "assign with expression" "X = 1 + 2"
            (Assign [ ('X', Binop (Add, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 + 2 + 3"
            (Assign
               [ ('X', Binop (Add, Binop (Add, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Sub",
        [
          instr_test_case "assign with expression" "X = 1 - 2"
            (Assign [ ('X', Binop (Sub, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 - 2 - 3"
            (Assign
               [ ('X', Binop (Sub, Binop (Sub, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Mul",
        [
          instr_test_case "assign with expression" "X = 1 * 2"
            (Assign [ ('X', Binop (Mul, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 * 2 * 3"
            (Assign
               [ ('X', Binop (Mul, Binop (Mul, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Div",
        [
          instr_test_case "assign with expression" "X = 1 / 2"
            (Assign [ ('X', Binop (Div, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 / 2 / 3"
            (Assign
               [ ('X', Binop (Div, Binop (Div, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Integer operations precedence",
        [
          instr_test_case "assign with expression" "X = 1 + 2 * 3"
            (Assign
               [ ('X', Binop (Add, Number 1, Binop (Mul, Number 2, Number 3))) ]);
          instr_test_case "assign with expression" "X = 1 * 2 + 3"
            (Assign
               [ ('X', Binop (Add, Binop (Mul, Number 1, Number 2), Number 3)) ]);
          instr_test_case "assign with expression" "X = 1 * 2 + 3 * 4"
            (Assign
               [
                 ( 'X',
                   Binop
                     ( Add,
                       Binop (Mul, Number 1, Number 2),
                       Binop (Mul, Number 3, Number 4) ) );
               ]);
          instr_test_case "assign with expression" "X = 1 + 2 * 3 + 4"
            (Assign
               [
                 ( 'X',
                   Binop
                     ( Add,
                       Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                       Number 4 ) );
               ]);
        ] );
      ( "Parenthesis in binary operations",
        [
          instr_test_case "assign with expression" "X = (1 + 2) * 3"
            (Assign
               [ ('X', Binop (Mul, Binop (Add, Number 1, Number 2), Number 3)) ]);
          instr_test_case "assign with expression" "X = 1 * (2 + 3)"
            (Assign
               [ ('X', Binop (Mul, Number 1, Binop (Add, Number 2, Number 3))) ]);
          instr_test_case "assign with expression" "X = (1 + 2) * (3 + 4)"
            (Assign
               [
                 ( 'X',
                   Binop
                     ( Mul,
                       Binop (Add, Number 1, Number 2),
                       Binop (Add, Number 3, Number 4) ) );
               ]);
          instr_test_case "assign with expression" "X = (1 + 2) * 3 + 4"
            (Assign
               [
                 ( 'X',
                   Binop
                     ( Add,
                       Binop (Mul, Binop (Add, Number 1, Number 2), Number 3),
                       Number 4 ) );
               ]);
          instr_test_case "assign with expression" "X = 1 + (2 * 3) + 4"
            (Assign
               [
                 ( 'X',
                   Binop
                     ( Add,
                       Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                       Number 4 ) );
               ]);
        ] );
      ( "Parenthesis in unary operations",
        [
          instr_test_case "assign with expression" "X = -(1 + 2)"
            (Assign [ ('X', Unop (Neg, Binop (Add, Number 1, Number 2))) ]);
          instr_test_case "assign with expression" "X = +(1 + 2)"
            (Assign [ ('X', Unop (Pos, Binop (Add, Number 1, Number 2))) ]);
        ] );
      ( "REM",
        [
          instr_test_case "assign with expression" "REM \"yes\"" (Rem "yes");
          fail_instr_test_case "assign with expression" "REM \"yes\" \"no\"";
          fail_instr_test_case "assign with expression" "REM yes";
          fail_instr_test_case "assign with expression" "REM 1";
        ] );
      ( "VAVERS",
        [
          fail_instr_test_case "Empty vavers" "VAVERS";
          instr_test_case "vavers integer" "VAVERS 1" (Vavers (Number 1));
          instr_test_case "vavers variable" "VAVERS X" (Vavers (Var 'X'));
          fail_instr_test_case "vavers string" "VAVERS \"1\"";
          instr_test_case "vavers expression" "VAVERS 1 + 2"
            (Vavers (Binop (Add, Number 1, Number 2)));
          instr_test_case "Complex expression" "VAVERS 1 + 2 * 3 + 4"
            (Vavers
               (Binop
                  ( Add,
                    Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                    Number 4 )));
        ] );
      ( "SI ALORS",
        [
          fail_instr_test_case "Incomplete SI" "SI";
          fail_instr_test_case "Incomplete ALORS" "ALORS";
          fail_instr_test_case "Incomplete SI ALORS" "SI ALORS";
          fail_instr_test_case "Incomplete SI ALORS" "SI 1 = 1 ALORS";
          fail_instr_test_case "Incomplete SI ALORS" "SI ALORS NL";
          instr_test_case "Simple SI Eq ALORS MULTIASSIGN"
            "SI 1 = 2 ALORS X = 2"
            (SiAlors (Eq, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Gt ALORS MULTIASSIGN"
            "SI 1 > 2 ALORS X = 2"
            (SiAlors (Gt, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Ge ALORS MULTIASSIGN"
            "SI 1 >= 2 ALORS X = 2"
            (SiAlors (Ge, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Lt ALORS MULTIASSIGN"
            "SI 1 < 2 ALORS X = 2"
            (SiAlors (Lt, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Le ALORS MULTIASSIGN"
            "SI 1 <= 2 ALORS X = 2"
            (SiAlors (Le, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Ne1 ALORS MULTIASSIGN"
            "SI 1 <> 2 ALORS X = 2"
            (SiAlors (Ne, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Ne2 ALORS MULTIASSIGN"
            "SI 1 >< 2 ALORS X = 2"
            (SiAlors (Ne, Number 1, Number 2, Assign [ ('X', Number 2) ]));
          instr_test_case "Nested SI Le ALORS [SI Ne ALORS ASSIGN]"
            "SI -1 <= 1 ALORS SI 1 <> 2 ALORS X = 200"
            (SiAlors
               ( Le,
                 Unop (Neg, Number 1),
                 Number 1,
                 SiAlors (Ne, Number 1, Number 2, Assign [ ('X', Number 200) ])
               ));
          instr_test_case "Simple SI Eq ALORS VAVERS" "SI 1 = 2 ALORS VAVERS 2"
            (SiAlors (Eq, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Simple SI Gt ALORS VAVERS" "SI 1 > 2 ALORS VAVERS 2"
            (SiAlors (Gt, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Simple SI Ge ALORS VAVERS" "SI 1 >= 2 ALORS VAVERS 2"
            (SiAlors (Ge, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Simple SI Lt ALORS VAVERS" "SI 1 < 2 ALORS VAVERS 2"
            (SiAlors (Lt, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Simple SI Le ALORS VAVERS" "SI 1 <= 2 ALORS VAVERS 2"
            (SiAlors (Le, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Simple SI Ne1 ALORS VAVERS"
            "SI 1 <> 2 ALORS VAVERS 2"
            (SiAlors (Ne, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Simple SI Ne2 ALORS VAVERS"
            "SI 1 >< 2 ALORS VAVERS 2"
            (SiAlors (Ne, Number 1, Number 2, Vavers (Number 2)));
          instr_test_case "Nested SI Le ALORS [SI Eq ALORS VAVERS]"
            "SI -1 <= 1 ALORS SI 1 <> 2 ALORS VAVERS 200"
            (SiAlors
               ( Le,
                 Unop (Neg, Number 1),
                 Number 1,
                 SiAlors (Ne, Number 1, Number 2, Vavers (Number 200)) ));
        ] );
      ( "ENTREE",
        [
          fail_instr_test_case "Empty entree" "ENTREE";
          fail_instr_test_case "ENTREE ," "ENTREE ,";
          instr_test_case "entree X" "ENTREE X" (Entree [ 'X' ]);
          fail_instr_test_case "entree x" "ENTREE x";
          fail_instr_test_case "entree \"X\"" "ENTREE \"X\"";
          fail_instr_test_case "entree XY" "ENTREE XY";
          fail_instr_test_case "entree X Y" "ENTREE X Y";
          instr_test_case "entree X Y" "ENTREE X, Y" (Entree [ 'X'; 'Y' ]);
          instr_test_case "entree X X" "ENTREE X, X" (Entree [ 'X'; 'X' ]);
          instr_test_case "entree X Y Z" "ENTREE X, Y, Z"
            (Entree [ 'X'; 'Y'; 'Z' ]);
          instr_test_case "entree X Y X" "ENTREE X, Y, X"
            (Entree [ 'X'; 'Y'; 'X' ]);
        ] );
      ( "IMPRIME",
        [
          fail_instr_test_case "Empty imprime" "IMPRIME";
          instr_test_case "imprime integer" "IMPRIME 1"
            (Imprime [ Expression (Number 1) ]);
          instr_test_case "imprime variable" "IMPRIME X"
            (Imprime [ Expression (Var 'X') ]);
          fail_instr_test_case "imprime string" "IMPRIME \"1\"";
          instr_test_case "imprime expression" "IMPRIME 1 + 2"
            (Imprime [ Expression (Binop (Add, Number 1, Number 2)) ]);
          instr_test_case "Complex expression" "IMPRIME 1 + 2 * 3 + 4"
            (Imprime
               [
                 Expression
                   (Binop
                      ( Add,
                        Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                        Number 4 ));
               ]);
          instr_test_case "Complex expression with String_"
            "IMPRIME 1 + 2 * 3 + 4, \"prout\""
            (Imprime
               [
                 Expression
                   (Binop
                      ( Add,
                        Binop (Add, Number 1, Binop (Mul, Number 2, Number 3)),
                        Number 4 ));
                 String_ "prout";
               ]);
        ] );
      ( "Sousroutine",
        [
          fail_instr_test_case "Empty sousroutine" "SOUSROUTINE";
          instr_test_case "sousroutine integer" "SOUSROUTINE 1"
            (Sousroutine (Number 1));
          instr_test_case "sousroutine variable" "SOUSROUTINE X"
            (Sousroutine (Var 'X'));
          fail_instr_test_case "sousroutine string" "SOUSROUTINE \"1\"";
          instr_test_case "sousroutine expression" "SOUSROUTINE 1 + 2"
            (Sousroutine (Binop (Add, Number 1, Number 2)));
        ] );
      ( "Retourne",
        [
          instr_test_case "retourne" "RETOURNE" Retourne;
          fail_instr_test_case "retourne 1" "RETOURNE 1";
          fail_instr_test_case "retourne string" "RETOURNE \"STRING\"";
        ] );
      ("FIN", [ instr_test_case "FIN" "FIN" Fin ]);
      ("NL", [ instr_test_case "NL" "NL" Nl ]);
      ( "Line",
        [
          program_test_case "non CR terminated line" "0 X = 1" None;
          program_test_case "CR terminated line" "0 X = 1 \n"
            (Some [ { number = 0; instr = Assign [ ('X', Number 1) ] } ]);
          program_test_case "Nultiple lines" "0 X = 1\n 10 Y = 2\n"
            (Some
               [
                 { number = 0; instr = Assign [ ('X', Number 1) ] };
                 { number = 10; instr = Assign [ ('Y', Number 2) ] };
               ]);
        ] );
      ("Program", [ program_test_case "empty program" "" (Some []) ]);
    ]
