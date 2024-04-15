open Latsi.Interpreter

let eval_str program =
  let lexbuf = Lexing.from_string program in
  let ast = Latsi.Parser.input Latsi.Lexer.lexer lexbuf in
  eval ast

let%expect_test "One IMPRIME, one expr" =
  eval_str "0 IMPRIME \"prout\"\n";
  [%expect {| prout |}]

let%expect_test "One IMPRIME, multiple expr" =
  eval_str "0 IMPRIME -3, \"prout\", 10\n";
  [%expect {| -3prout10 |}]

let%expect_test "Multiple IMPRIME, one expr" =
  eval_str "0 IMPRIME -1*-1\n1 IMPRIME -1*-2\n";
  [%expect {| 12 |}]

let%expect_test "Multiple IMPRIME, multiple expr" =
  eval_str
    "0 IMPRIME -1*-1, \"prout de fou\"\n1 IMPRIME -1*-2, \"splash de fou\"\n";
  [%expect {| 1prout de fou2splash de fou |}]

let%expect_test "FIN; IMPRIME" =
  eval_str "0 FIN\n1 IMPRIME \"Gandalf\"\n";
  [%expect {||}]

let%expect_test "IMPRIME; FIN IMPRIME" =
  eval_str "0 IMPRIME \"Gandalf\"\n1 FIN\n2 IMPRIME \"Olorin\"\n";
  [%expect {| Gandalf |}]
