type token =
  | Nat of int
  | String of string
  | Var of char
  | Langle
  | Rangle
  | LangleEqual
  | RangleEqual
  | NotEqual
  | Equal
  | Plus
  | Minus
  | Mult
  | Div
  | EOF

let to_string = function
  | Nat n -> "Nat " ^ string_of_int n
  | String s -> "String " ^ s
  | Var c -> "Var " ^ String.make 1 c
  | Langle -> "Langle"
  | Rangle -> "Rangle"
  | LangleEqual -> "LangleEqual"
  | RangleEqual -> "RangleEqual"
  | NotEqual -> "NotEqual"
  | Equal -> "Equal"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Div -> "Div"
  | EOF -> "EOF"
