open Latsi_extension.Parser
open Latsi_extension.Token
open Latsi_extension.Ast
open Alcotest

let keywords =
  [ "NL"; "REM"; "VAVERS"; "SI"; "ALORS"; "ENTREE"; "IMPRIME"; "FIN" ]

(* This function should be as fast as possible, it's a bottle nec for arbitrary_var. This
   solutions is not the purest, but it's pretty fast. *)
let join_char_list (l : char list) =
  let buf = Buffer.create 64 in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

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

let generator_var_start =
  let open QCheck in
  Gen.oneof [ Gen.char_range 'a' 'z'; Gen.char_range 'A' 'Z' ]

let generator_var_char =
  let open QCheck in
  Gen.oneof
    (Gen.return '_'
    :: [
         Gen.char_range 'a' 'z'; Gen.char_range 'A' 'Z'; Gen.char_range '0' '9';
       ])

let generator_var =
  let open QCheck in
  Gen.map2
    (fun c s ->
      let s = String.make 1 c ^ s in
      if List.mem s keywords then s ^ "_" else s)
    generator_var_start
    (Gen.map (fun l -> join_char_list l) (Gen.list generator_var_char))

let arbitrary_var =
  let open QCheck in
  make generator_var ~print:(Format.asprintf "%s")

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
