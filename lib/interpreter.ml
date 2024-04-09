open Ast

exception Empty_program
exception Unkwown_line_number

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
    | Rem _ -> None
    | Assign (x, e) ->
        let vx = eval_expression env e in
        Hashtbl.replace env x vx;
        None
    | Vavers e ->
        let index = eval_expression env e in
        Some index

  let rec eval_program env cur program =
    match cur with
    | None -> ()
    | Some cur -> (
        (* TODO: Refactor this *)
        match Hashtbl.find_opt program cur with
        | None -> raise Unkwown_line_number
        | Some (instr, next) -> (
            match eval_instruction env instr with
            | None -> eval_program env next program
            | Some next -> eval_program env (Some next) program))

  let eval_env program =
    match program with
    | [] -> raise Empty_program
    | _ ->
        (* Initialize the environment with 26 variables A-Z *)
        let env = Hashtbl.create 26 in
        List.init 26 (fun i -> char_of_int (i + int_of_char 'A'))
        |> List.iter (fun v -> Hashtbl.add env v 0);

        (* Remove duplicates *)
        let program_hashtable = Hashtbl.create (List.length program) in
        List.iter
          (fun { number; instr } ->
            Hashtbl.replace program_hashtable number instr)
          program;

        (* Sort the program by line number *)
        let program =
          Hashtbl.to_seq program_hashtable
          |> List.of_seq
          |> List.sort (fun (number1, _) (number2, _) ->
                 compare number1 number2)
        in

        (* Setup the program hash table, which allows us to always have the
           next instruction and make jumps easier *)
        let program_intrs = Hashtbl.create (List.length program) in

        let start =
          program |> List.rev
          |> List.fold_left
               (fun acc (number, instr) ->
                 Hashtbl.add program_intrs number (instr, acc);
                 Some number)
               None
        in

        (* Evaluate the program *)
        eval_program env start program_intrs;
        env

  let value_of var env = Hashtbl.find env var
  let is_correct_value var value env = Hashtbl.find env var = value
end

let eval program =
  let _ = Implementation.eval_env program in
  ()
