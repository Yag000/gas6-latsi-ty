type variable = char
type unop = Pos | Neg

type binop = Add | Sub | Mul | Div

and expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Var of variable
  | Number of int

type instruction = Assign of variable * expression | Rem of string
type line = { number : int; instr : instruction }
type program = line list

let rec equal_expression e e' =
  match (e, e') with
  | Binop (op, e1, e2), Binop (op', e1', e2') ->
      op = op' && equal_expression e1 e1' && equal_expression e2 e2'
  | Unop (u, e), Unop (u', e') -> u = u' && equal_expression e e'
  | Var v, Var v' -> v = v'
  | Number n, Number n' -> n = n'
  | _ -> false

let equal_instruction i i' =
  match (i, i') with
  | Assign (v, e), Assign (v', e') -> v = v' && equal_expression e e'
  | Rem s, Rem s' -> s = s'
  | _ -> false

let equal_program p1 p2 =
  List.length p1 = List.length p2
  && List.for_all2
       (fun l1 l2 ->
         l1.number = l2.number && equal_instruction l1.instr l2.instr)
       p1 p2

let pp_unop ff = function
  | Pos -> Format.fprintf ff "+"
  | Neg -> Format.fprintf ff "-"

let pp_binop ff = function
  | Add -> Format.fprintf ff "+"
  | Sub -> Format.fprintf ff "-"
  | Mul -> Format.fprintf ff "*"
  | Div -> Format.fprintf ff "/"

let rec pp_expression ff = function
  | Var v -> Format.fprintf ff "%c" v
  | Number n -> Format.fprintf ff "%d" n
  | Unop (u, e) -> Format.fprintf ff "(%a%a)" pp_unop u pp_expression e
  | Binop (op, e1, e2) ->
      Format.fprintf ff "(%a %a %a)" pp_expression e1 pp_binop op pp_expression
        e2

let pp_instruction ff = function
  | Assign (v, e) -> Format.fprintf ff "%c = %a" v pp_expression e
  | Rem s -> Format.fprintf ff "REM %s" s

let pp_line ff l = Format.fprintf ff "%d %a" l.number pp_instruction l.instr

let pp_titled_list (title : string) ff (l : 'a list) pp =
  Format.fprintf ff "%s" title;
  match l with
  | [] -> Format.fprintf ff ": []@."
  | _ ->
      Format.fprintf ff ": [@.@[<v>%a@]@.]@."
        Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp)
        l

let pp_program ff p = pp_titled_list "Program" ff p pp_line
