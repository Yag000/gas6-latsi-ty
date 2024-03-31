open Ast

val eval : program -> unit

module Implementation : sig
  type env

  val eval_env : program -> env
  val value_of : variable -> env -> int
  val is_correct_value : variable -> int -> env -> bool
end
