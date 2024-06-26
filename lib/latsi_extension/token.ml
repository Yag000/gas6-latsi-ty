open Parser

let token_to_string = function
  | Nat n -> "Nat " ^ string_of_int n
  | String s -> "String " ^ s
  | Var v -> "Var " ^ v
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
  | Si -> "Si"
  | Alors -> "Alors"
  | Entree -> "Entree"
  | Comma -> "Comma"
  | Imprime -> "Imprime"
  | Sousroutine -> "Sousroutine"
  | Retourne -> "Retourne"
  | Fin -> "Fin"
  | Nl -> "Nl"
  | CR -> "CR"
  | EOF -> "EOF"
