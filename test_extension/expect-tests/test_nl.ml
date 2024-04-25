open Utils

let%expect_test "One NL" =
  eval_str "0 NL\n";
  [%expect_exact {|
|}]

let%expect_test "IMPRIME; NL; IMPRIME" =
  eval_str "0 IMPRIME -1*-1, \"Varda\"\n1 NL\n2 IMPRIME -1*-2, \"Elentari\"\n";
  [%expect_exact {|1Varda
2Elentari|}]

let%expect_test "IMPRIME; NL; NL; IMPRIME" =
  eval_str
    "0 IMPRIME -1*-1, \"Yavanna\"\n1 NL\n2 NL\n3 IMPRIME -1*-2, \"Kementari\"\n";
  [%expect_exact {|1Yavanna

2Kementari|}]

let%expect_test "NL; IMPRIME; NL; IMPRIME; NL" =
  eval_str
    "0 NL\n\
     1 IMPRIME -1*-1, \"Yavanna\"\n\
     2 NL\n\
     3 IMPRIME -1*-2, \"Kementari\"\n\
     4 NL\n";
  [%expect_exact {|
1Yavanna
2Kementari
|}]

let%expect_test "FIN; NL" =
  eval_str "0 FIN\n1 NL\n";
  [%expect_exact {||}]

let%expect_test "NL; IMPRIME; FIN; NL; IMPRIME" =
  eval_str
    "0 NL\n\
     1 IMPRIME -1*-1, \"Yavanna\"\n\
     2 FIN\n\
     3 NL\n\
     4 IMPRIME -1*-2, \"Kementari\"\n";
  [%expect_exact {|
1Yavanna|}]
