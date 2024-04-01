open Latsi.Token
open Alcotest

let join_char_list (l : char list) =
  List.fold_left (fun acc c -> acc ^ String.make 1 c) "" l

let pp_token ff (token : token) = Format.fprintf ff "%s" (to_string token)

let token_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_token)) ( = )

let char_gen_list_from_string (str : string) =
  str |> String.to_seq |> List.of_seq |> List.map (fun c -> QCheck.Gen.return c)

let generator_custom_char =
  let open QCheck in
  Gen.oneof
    (char_gen_list_from_string " ,'_;:()."
    @ [ Gen.char_range 'a' 'z'; Gen.char_range 'A' 'Z' ])

let pp_custom_char ff c =
  Format.fprintf ff "%c" c

let arbitrary_custom_char = QCheck.make ~print:(Format.asprintf "%a" pp_custom_char) generator_custom_char

let arbitrary_var = 
  let open QCheck in
    make (Gen.char_range 'A' 'Z')


let pp_token ff token = 
  Format.fprintf ff "%s" (to_string token)

let pp_list ff (l : 'a list) pp =
  match l with
  | [] -> Format.fprintf ff "[]"
  | _ ->
      Format.fprintf ff "[@[<hov>%a@]]@."
        Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp)
        l

let pp_token_list ff (tl : token list) =
  pp_list ff tl pp_token