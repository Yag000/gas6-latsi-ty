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

val to_string : token -> string