open Latsi.Ast
open Latsi.Interpreter

let%expect_test "One IMRPIME, multiple expr" =
  let program =
    [
      {
        number = 0;
        instr =
          Imprime
            [
              Expression (Binop (Mul, Number (-1), Number 3));
              String_ "prout";
              Expression (Binop (Add, Number (-5), Number 15));
            ];
      };
    ]
  in
  eval program;
  [%expect {|-3prout10|}]

let%expect_test "One IMRPIME, one expr" =
  let program =
    [
      {
        number = 0;
        instr =
          Imprime
            [
              String_ "prout"
            ];
      };
    ]
  in
  eval program;
  [%expect {|prout|}]

let%expect_test "Multi IMRPIME, multiple expr" =
  let program =
    [
      {
        number = 0;
        instr =
          Imprime
            [
              Expression (Binop (Mul, Number (-1), Number (-1)));
              String_ "prout splash";
              Expression (Number 9999);
            ];
      };
      {
        number = 1;
        instr =
          Imprime
            [
              Expression (Binop (Add, Number 1, Number 1));
              String_ "prout splash";
              Expression (Number 9999);
            ];
      };
      {
        number = 2;
        instr =
          Imprime
            [
              Expression (Binop (Div, Number 6, Number 2));
              String_ "prout splash";
              Expression (Number 9999);
            ];
      };
    ]
  in
  eval program;
  [%expect "
1prout splash9999
2prout splash9999
3prout splash9999"]
