open Latsi_extension.Parser
open Latsi_extension.Token
open Latsi_extension.Ast
open Alcotest

let join_char_list (l : char list) =
  List.fold_left (fun acc c -> acc ^ String.make 1 c) "" l

let pp_token ff (token : token) = Format.fprintf ff "%s" (token_to_string token)

let token_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_token)) ( = )

let char_gen_list_from_string (str : string) =
  str |> String.to_seq |> List.of_seq |> List.map (fun c -> QCheck.Gen.return c)

let generator_custom_char =
  let open QCheck in
  Gen.oneof
    (char_gen_list_from_string " ,'_;:()."
    @ [ Gen.char_range 'a' 'z'; Gen.char_range 'A' 'Z' ])

let pp_custom_char ff c = Format.fprintf ff "%c" c

let arbitrary_custom_char =
  QCheck.make ~print:(Format.asprintf "%a" pp_custom_char) generator_custom_char

let arbitrary_var =
  let open QCheck in
  make (Gen.char_range 'A' 'Z')

let pp_list ff (l : 'a list) pp =
  match l with
  | [] -> Format.fprintf ff "[]"
  | _ ->
      Format.fprintf ff "[@[<hov>%a@]]@."
        Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp)
        l

let pp_token_list ff (tl : token list) = pp_list ff tl pp_token

let program_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_program)) equal_program

let generator_number =
  let open QCheck in
  Gen.map (fun n -> Number n) (Gen.int_range 0 100)

let arbitrary_number =
  QCheck.make ~print:(Format.asprintf "%a" pp_expression) generator_number

let generator_assign =
  let open QCheck in
  pair arbitrary_var arbitrary_number

let pp_assign_debug ff (v, e) =
  Format.fprintf ff "ASSIGN: %c = %a" v pp_expression e

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
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_print_char)
        vl pp_relop Eq
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_expression)
        el
  | SiAlors (r, e1, e2, i) ->
      Format.fprintf ff "SI [%a] %a [%a] ALORS [%a]" pp_expression e1 pp_relop r
        pp_expression e2 pp_instruction_debug i
  | Rem s -> Format.fprintf ff "REM %s" s
  | Vavers e -> Format.fprintf ff "VAVERS %a" pp_expression e
  | Entree l ->
      Format.fprintf ff "ENTREE @[<hov>%a@]"
        Format.(pp_print_list ~pp_sep:sep_soft_comma pp_print_char)
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
