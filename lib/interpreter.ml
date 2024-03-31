open Ast

module Implementation = struct
  type env = (char, int) Hashtbl.t

  let eval_binop = function
    | Add -> ( + )
    | Sub -> ( - )
    | Mul -> ( * )
    | Div -> ( / )

  let rec eval_expression env = function
    | Binop (op, e1, e2) ->
        let v1 = eval_expression env e1 in
        let v2 = eval_expression env e2 in
        eval_binop op v1 v2
    | Unop (Neg, e) -> -eval_expression env e
    | Unop (Pos, e) -> eval_expression env e
    | Number n -> n
    | Var x -> Hashtbl.find env x

  let eval_instruction env = function
    | Rem _ -> ()
    | Assign (x, e) ->
        let vx = eval_expression env e in
        Hashtbl.replace env x vx

  let eval_line env { instr; _ } = eval_instruction env instr

  let eval_env program =
    let env = Hashtbl.create 26 in
    (* Initialize the environment with 26 variables A-Z *)
    List.init 26 (fun i -> char_of_int (i + int_of_char 'A'))
    |> List.iter (fun v -> Hashtbl.add env v 0);
    List.iter (eval_line env) program;
    env

  let value_of var env = Hashtbl.find env var
  let is_correct_value var value env = Hashtbl.find env var = value
end

let eval program =
  let _ = Implementation.eval_env program in
  ()
