open Ast

exception Empty_program
exception Unkwown_line_number
exception Illegal_return

val eval : program -> unit
(** Evaluates a program, i.e. computes the final value of each variable
    @raise Empty_program if the program is empty
    @raise Unkwown_line_number if a [Vavers] points to a non existent line

 *)

(* Implementation details, only exported for testing purposes *)
module Implementation : sig
  type env
  type input_method = Stdin | Ints of int list

  val eval_env : ?input:input_method -> program -> env
  val value_of : variable -> env -> int
  val is_correct_value : variable -> int -> env -> bool
end
