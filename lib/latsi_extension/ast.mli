exception ParserError

type variable = char
type unop = Pos | Neg
type binop = Add | Sub | Mul | Div

type relop = Lt | Gt | Le | Ge | Ne | Eq

and expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Var of variable
  | Number of int

type expr = String_ of string | Expression of expression
type assign = variable * expression

type instruction =
  | Imprime of expr list
  | Assign of assign list
  | SplitAssign of variable list * expression list
  | Rem of string
  | Vavers of expression
  | SiAlors of relop * expression * expression * instruction
  | Entree of variable list
  | Sousroutine of expression
  | Retourne
  | Fin
  | Nl

type line = { number : int; instr : instruction }
type program = line list

val equal_program : program -> program -> bool
val sep_soft_comma : Format.formatter -> unit -> unit
val pp_relop : Format.formatter -> relop -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_program : Format.formatter -> program -> unit
val pp_expression : Format.formatter -> expression -> unit
