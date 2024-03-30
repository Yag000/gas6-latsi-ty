type variable = char

type expression =
  | Add of expression option * expression
  | Sub of expression option * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Var of variable
  | Number of int

type instruction = Assing of variable * expression | Rem of string
type line = { number : int; instr : instruction }
type program = line list

val equal_program : program -> program -> bool
val pp_program : Format.formatter -> program -> unit
