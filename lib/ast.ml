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

let pp_unop fmt = function
  | Pos -> Format.fprintf fmt "+"
  | Neg -> Format.fprintf fmt "-"

let pp_binop fmt = function
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"
  | Mul -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"

let rec pp_expression fmt = function
  | Var v -> Format.fprintf fmt "%c" v
  | Number n -> Format.fprintf fmt "%d" n
  | Unop (u, e) -> Format.fprintf fmt "(%a%a)" pp_unop u pp_expression e
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" pp_expression e1 pp_binop op pp_expression
        e2

let pp_instruction fmt = function
  | Assign (v, e) -> Format.fprintf fmt "%c = %a" v pp_expression e
  | Rem s -> Format.fprintf fmt "REM %s" s

let pp_line fmt l = Format.fprintf fmt "%d: %a" l.number pp_instruction l.instr

let pp_program fmt p =
  List.iter (fun l -> Format.fprintf fmt "%a@." pp_line l) p
