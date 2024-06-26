exception ParserError

type variable = string
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
  | Rem of string (* TODO: Add remaining constructors *)
  | Vavers of expression
  | SiAlors of relop * expression * expression * instruction
  | Entree of variable list
  | Sousroutine of expression
  | Retourne
  | Fin
  | Nl

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

let equal_assign (v, e) (v', e') = v = v' && equal_expression e e'

let rec equal_instruction i i' =
  match (i, i') with
  | Imprime el, Imprime el' -> el = el'
  | Assign al, Assign al' ->
      List.for_all2 (fun a a' -> equal_assign a a') al al'
  | SplitAssign (vl, el), SplitAssign (vl', el') ->
      List.compare_lengths vl vl' = 0
      && List.compare_lengths el el' = 0
      && vl = vl'
      && List.for_all2 equal_expression el el'
  | Rem s, Rem s' -> s = s'
  | Vavers e, Vavers e' -> equal_expression e e'
  | Entree l, Entree l' -> l = l'
  | Fin, Fin -> true
  | Nl, Nl -> true
  | SiAlors (r, e1, e2, i), SiAlors (r', e1', e2', i') ->
      r = r' && equal_expression e1 e1' && equal_expression e2 e2'
      && equal_instruction i i'
  | Sousroutine e, Sousroutine e' -> equal_expression e e'
  | Retourne, Retourne -> true
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
  | Var v -> Format.fprintf ff "%s" v
  | Number n -> Format.fprintf ff "%d" n
  | Unop (u, e) -> Format.fprintf ff "(%a%a)" pp_unop u pp_expression e
  | Binop (op, e1, e2) ->
      Format.fprintf ff "(%a %a %a)" pp_expression e1 pp_binop op pp_expression
        e2

let pp_expr ff = function
  | String_ s -> Format.fprintf ff "%s" s
  | Expression e -> pp_expression ff e

let pp_relop ff = function
  | Lt -> Format.fprintf ff "<"
  | Gt -> Format.fprintf ff ">"
  | Le -> Format.fprintf ff "<="
  | Ge -> Format.fprintf ff ">="
  | Ne -> Format.fprintf ff "<>"
  | Eq -> Format.fprintf ff "="

let pp_assign ff (v, e) = Format.fprintf ff "%s = %a" v pp_expression e
let sep_soft_comma out () = Format.fprintf out ",@ "

let rec pp_instruction ff = function
  | Imprime el ->
      Format.fprintf ff "IMPRIME [@[<h>%a@]]"
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_expr)
        el
  | Assign l ->
      Format.fprintf ff "@[<hov>%a@]"
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_assign)
        l
  | SplitAssign (vl, el) ->
      Format.fprintf ff "@[<hov>%a@] %a @[<hov>%a@]"
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_print_string)
        vl pp_relop Eq
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_expression)
        el
  | SiAlors (r, e1, e2, i) ->
      Format.fprintf ff "SI [%a] %a [%a] ALORS [%a]" pp_expression e1 pp_relop r
        pp_expression e2 pp_instruction i
  | Rem s -> Format.fprintf ff "REM %s" s
  | Vavers e -> Format.fprintf ff "VAVERS %a" pp_expression e
  | Entree l ->
      Format.fprintf ff "ENTREE @[<hov>%a@]"
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_print_string)
        l
  | Sousroutine e -> Format.fprintf ff "SOUSROUTINE %a" pp_expression e
  | Retourne -> Format.fprintf ff "RETOURNE"
  | Fin -> Format.fprintf ff "FIN"
  | Nl -> Format.fprintf ff "NL"

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

module Debug = struct
  let pp_assign_debug ff (v, e) =
    Format.fprintf ff "ASSIGN: %s = %a" v pp_expression e

  let rec pp_instruction_debug ff = function
    | Imprime el ->
        Format.fprintf ff "IMPRIME [@[<h>%a@]]"
          Format.(pp_print_list ~pp_sep:sep_soft_comma pp_expr)
          el
    | Assign l ->
        Format.fprintf ff "@[<hov>%a@]"
          Format.(pp_print_list ~pp_sep:sep_soft_comma pp_assign_debug)
          l
    | SplitAssign (vl, el) ->
        Format.fprintf ff "SPLIT ASSIGN: @[<hov>%a@] %a @[<hov>%a@]"
          Format.(pp_print_list ~pp_sep:sep_soft_comma pp_print_string)
          vl pp_relop Eq
          Format.(pp_print_list ~pp_sep:sep_soft_comma pp_expression)
          el
    | SiAlors (r, e1, e2, i) ->
        Format.fprintf ff "SI [%a] %a [%a] ALORS [%a]" pp_expression e1 pp_relop
          r pp_expression e2 pp_instruction_debug i
    | Rem s -> Format.fprintf ff "REM %s" s
    | Vavers e -> Format.fprintf ff "VAVERS %a" pp_expression e
    | Entree l ->
        Format.fprintf ff "ENTREE @[<hov>%a@]"
          Format.(pp_print_list ~pp_sep:sep_soft_comma pp_print_string)
          l
    | Sousroutine e -> Format.fprintf ff "SOUSROUTINE %a" pp_expression e
    | Retourne -> Format.fprintf ff "RETOURNE"
    | Fin -> Format.fprintf ff "FIN"
    | Nl -> Format.fprintf ff "NL"

  let pp_line_debug ff l =
    Format.fprintf ff "%d %a" l.number pp_instruction_debug l.instr

  let pp_titled_list (title : string) ff (l : 'a list) pp =
    Format.fprintf ff "%s" title;
    match l with
    | [] -> Format.fprintf ff ": []@."
    | _ ->
        Format.fprintf ff ": [@.@[<v>%a@]@.]@."
          Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp)
          l

  let pp_program_debug ff p = pp_titled_list "Program" ff p pp_line_debug
end
