open Ast

let rec eval_expression env = function
  | Add (Some e1, e2) -> eval_expression env e1 + eval_expression env e2
  | Add (None, e2) -> eval_expression env e2
  | Sub (Some e1, e2) -> eval_expression env e1 - eval_expression env e2
  | Sub (None, e2) -> -eval_expression env e2
  | Mul (e1, e2) -> eval_expression env e1 * eval_expression env e2
  | Div (e1, e2) -> eval_expression env e1 / eval_expression env e2
  | Number n -> n
  | Var x -> Hashtbl.find env x

let eval_instruction env = function
  | Rem _ -> ()
  | Assing (x, e) ->
      let vx = eval_expression env e in
      Hashtbl.replace env x vx

let eval_line env { instr; _ } = eval_instruction env instr

let eval =
  let env = Hashtbl.create 26 in
  (* Initialize the environment with 26 variables A-Z *)
  List.init 26 (fun i -> char_of_int (i + int_of_char 'A'))
  |> List.iter (fun v -> Hashtbl.add env v 0);
  List.iter (eval_line env)
