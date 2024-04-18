open Utils

let%expect_test "One NL" =
  eval_str "0 NL\n";
  [%expect_exact {|
|}]

let%expect_test "IMPRIME; NL; IMPRIME" =
  eval_str "0 IMPRIME -1*-1, \"Varda\"\n1 NL\n2 IMPRIME -1*-2, \"Elentari\"\n";
  [%expect {|
    1Varda
    2Elentari |}]

let%expect_test "IMPRIME; NL; NL; IMPRIME" =
  eval_str
    "0 IMPRIME -1*-1, \"Yavanna\"\n1 NL\n2 NL\n3 IMPRIME -1*-2, \"Kementari\"\n";
  [%expect {|
    1Yavanna

    2Kementari |}]
