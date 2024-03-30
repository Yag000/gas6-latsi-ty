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

let arbitrary_custom_char = QCheck.make generator_custom_char

let arbitrary_var = 
  let open QCheck in
    make (Gen.char_range 'A' 'Z')
