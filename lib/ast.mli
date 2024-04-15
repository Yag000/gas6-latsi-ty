type variable = char
type unop = Pos | Neg

type binop = Add | Sub | Mul | Div

and expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Var of variable
  | Number of int

type expr = String_ of string | Expression of expression

type instruction =
  | Imprime of expr list
  | Assign of variable * expression
  | Rem of string
  | Vavers of expression
  | Entree of variable list
  | Fin

type line = { number : int; instr : instruction }
type program = line list

val equal_program : program -> program -> bool
val pp_program : Format.formatter -> program -> unit
