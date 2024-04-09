open Parser

let token_to_string = function
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
  | Times -> "Mult"
  | Slash -> "Div"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Rem -> "Rem"
  | Vavers -> "Vavers"
  | CR -> "CR"
  | EOF -> "EOF"
