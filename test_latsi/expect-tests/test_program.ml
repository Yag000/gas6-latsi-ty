open Utils

let%expect_test "Simple program" =
  eval_str
    "5 REM \"Ce programme est formidable.\"\n\
     10 IMPRIME \"Bonjour Paris\"\n\
     15 NL\n\
     20 I = 0\n\
     30 SI I > 10 ALORS VAVERS 40\n\
     35 IMPRIME I, \" \"\n\
     37 I = I + 1\n\
     39 VAVERS 30\n\
     40 FIN\n\
     50 IMPRIME \"ne s'imprime pas\"\n";
  [%expect_exact {|Bonjour Paris
0 1 2 3 4 5 6 7 8 9 10 |}]

let%expect_test "Powers of 2" =
  eval_str
    "0 X = 1\n\
     10 IMPRIME X, \" \"\n\
     20 X = X * 2\n\
     30 SI X < 100 ALORS VAVERS 10\n\
     40 FIN\n";
  [%expect_exact {|1 2 4 8 16 32 64 |}]
