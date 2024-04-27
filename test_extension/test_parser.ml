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

let () =
  run "Parser"
    [
      ( "Assign",
        [
          instr_test_case "simple integer" "X = 1"
            (MultiAssign [ ('X', Number 1) ]);
          fail_instr_test_case "invalid variable" "x = 1";
          fail_instr_test_case "assigning a string" "X = \"1\"";
          instr_test_case "X = Y" "X = Y" (MultiAssign [ ('X', Var 'Y') ]);
          instr_test_case "X = X" "X = X" (MultiAssign [ ('X', Var 'X') ]);
        ] );
      ( "MultiAssign",
        [
          instr_test_case "2 integers" "X = 1, Y = 2"
            (MultiAssign [ ('X', Number 1); ('Y', Number 2) ]);
          fail_instr_test_case "one invalid variable" "x = 1, Y = 2";
          fail_instr_test_case "one assigning a string" "X = \"1\", Y = 2";
          fail_instr_test_case "invalid variables" "x = 1, y = 2";
          fail_instr_test_case "assigning strings" "X = \"1\", Y = \"2\"";
          instr_test_case "X = Y, Y = X" "X = Y, Y = X"
            (MultiAssign [ ('X', Var 'Y'); ('Y', Var 'X') ]);
          instr_test_case "X = X, Y = Y" "X = X, Y = Y"
            (MultiAssign [ ('X', Var 'X'); ('Y', Var 'Y') ]);
        ] );
      ( "Unary operations",
        [
          instr_test_case "positive integer alone" "X = +1"
            (MultiAssign [ ('X', Unop (Pos, Number 1)) ]);
          instr_test_case "negative integer alone" "X = -1"
            (MultiAssign [ ('X', Unop (Neg, Number 1)) ]);
        ] );
      ( "Sum",
        [
          instr_test_case "simple integer" "X = 1"
            (MultiAssign [ ('X', Number 1) ]);
          instr_test_case "assign with expression" "X = 1 + 2"
            (MultiAssign [ ('X', Binop (Add, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 + 2 + 3"
            (MultiAssign
               [ ('X', Binop (Add, Binop (Add, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Sub",
        [
          instr_test_case "assign with expression" "X = 1 - 2"
            (MultiAssign [ ('X', Binop (Sub, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 - 2 - 3"
            (MultiAssign
               [ ('X', Binop (Sub, Binop (Sub, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Mul",
        [
          instr_test_case "assign with expression" "X = 1 * 2"
            (MultiAssign [ ('X', Binop (Mul, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 * 2 * 3"
            (MultiAssign
               [ ('X', Binop (Mul, Binop (Mul, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Div",
        [
          instr_test_case "assign with expression" "X = 1 / 2"
            (MultiAssign [ ('X', Binop (Div, Number 1, Number 2)) ]);
          instr_test_case "assign with expression" "X = 1 / 2 / 3"
            (MultiAssign
               [ ('X', Binop (Div, Binop (Div, Number 1, Number 2), Number 3)) ]);
        ] );
      ( "Integer operations precedence",
        [
          instr_test_case "assign with expression" "X = 1 + 2 * 3"
            (MultiAssign
               [ ('X', Binop (Add, Number 1, Binop (Mul, Number 2, Number 3))) ]);
          instr_test_case "assign with expression" "X = 1 * 2 + 3"
            (MultiAssign
               [ ('X', Binop (Add, Binop (Mul, Number 1, Number 2), Number 3)) ]);
          instr_test_case "assign with expression" "X = 1 * 2 + 3 * 4"
            (MultiAssign
               [
                 ( 'X',
                   Binop
                     ( Add,
                       Binop (Mul, Number 1, Number 2),
                       Binop (Mul, Number 3, Number 4) ) );
               ]);
          instr_test_case "assign with expression" "X = 1 + 2 * 3 + 4"
            (MultiAssign
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
            (MultiAssign
               [ ('X', Binop (Mul, Binop (Add, Number 1, Number 2), Number 3)) ]);
          instr_test_case "assign with expression" "X = 1 * (2 + 3)"
            (MultiAssign
               [ ('X', Binop (Mul, Number 1, Binop (Add, Number 2, Number 3))) ]);
          instr_test_case "assign with expression" "X = (1 + 2) * (3 + 4)"
            (MultiAssign
               [
                 ( 'X',
                   Binop
                     ( Mul,
                       Binop (Add, Number 1, Number 2),
                       Binop (Add, Number 3, Number 4) ) );
               ]);
          instr_test_case "assign with expression" "X = (1 + 2) * 3 + 4"
            (MultiAssign
               [
                 ( 'X',
                   Binop
                     ( Add,
                       Binop (Mul, Binop (Add, Number 1, Number 2), Number 3),
                       Number 4 ) );
               ]);
          instr_test_case "assign with expression" "X = 1 + (2 * 3) + 4"
            (MultiAssign
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
            (MultiAssign [ ('X', Unop (Neg, Binop (Add, Number 1, Number 2))) ]);
          instr_test_case "assign with expression" "X = +(1 + 2)"
            (MultiAssign [ ('X', Unop (Pos, Binop (Add, Number 1, Number 2))) ]);
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
            (SiAlors (Eq, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Gt ALORS MULTIASSIGN"
            "SI 1 > 2 ALORS X = 2"
            (SiAlors (Gt, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Ge ALORS MULTIASSIGN"
            "SI 1 >= 2 ALORS X = 2"
            (SiAlors (Ge, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Lt ALORS MULTIASSIGN"
            "SI 1 < 2 ALORS X = 2"
            (SiAlors (Lt, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Le ALORS MULTIASSIGN"
            "SI 1 <= 2 ALORS X = 2"
            (SiAlors (Le, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Ne1 ALORS MULTIASSIGN"
            "SI 1 <> 2 ALORS X = 2"
            (SiAlors (Ne, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Simple SI Ne2 ALORS MULTIASSIGN"
            "SI 1 >< 2 ALORS X = 2"
            (SiAlors (Ne, Number 1, Number 2, MultiAssign [ ('X', Number 2) ]));
          instr_test_case "Nested SI Le ALORS [SI Ne ALORS ASSIGN]"
            "SI -1 <= 1 ALORS SI 1 <> 2 ALORS X = 200"
            (SiAlors
               ( Le,
                 Unop (Neg, Number 1),
                 Number 1,
                 SiAlors
                   (Ne, Number 1, Number 2, MultiAssign [ ('X', Number 200) ])
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
      ("FIN", [ instr_test_case "FIN" "FIN" Fin ]);
      ("NL", [ instr_test_case "NL" "NL" Nl ]);
      ( "Line",
        [
          program_test_case "non CR terminated line" "0 X = 1" None;
          program_test_case "CR terminated line" "0 X = 1 \n"
            (Some [ { number = 0; instr = MultiAssign [ ('X', Number 1) ] } ]);
          program_test_case "Nultiple lines" "0 X = 1\n 10 Y = 2\n"
            (Some
               [
                 { number = 0; instr = MultiAssign [ ('X', Number 1) ] };
                 { number = 10; instr = MultiAssign [ ('Y', Number 2) ] };
               ]);
        ] );
      ("Program", [ program_test_case "empty program" "" (Some []) ]);
    ]
