open Latsi.Token
open Latsi.Lexer
open Utils

let generate_token_list (str : string) =
  let lexbuf = Lexing.from_string str in
  let rec crawl_lexbuf acc (t : token) =
    match t with
    | EOF -> List.rev acc
    | _ -> crawl_lexbuf (t :: acc) (lexer lexbuf)
  in
  crawl_lexbuf [] (lexer lexbuf)

let test_token_lists (msg : string) (actual : token list)
    (expected : token list) =
  Alcotest.test_case msg `Quick (fun () ->
      Alcotest.(check (list token_testable)) "same list" actual expected)

let test_empty_tokenization =
  test_token_lists "test_Langle_tokenization" ("" |> generate_token_list) []

let test_String_tokenization =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Forall string x composed of custom_chars, String x is generated"
    (string_gen generator_custom_char) (fun str ->
      let token_list = generate_token_list ("\"" ^ str ^ "\"") in
      [ String str ] = token_list)

let test_Var_tokenization =
  let open QCheck in
  Test.make ~count:1000 ~name:"Forall x in [A-Z], Var x is generated"
    arbitrary_var (fun c -> [ Var c ] = (Char.escaped c |> generate_token_list))

let test_Nat_tokenization =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Forall strictly positive integers x, Nat x is generated" pos_int
    (fun n -> n |> string_of_int |> generate_token_list = [ Nat n ])

let test_Langle_tokenization =
  test_token_lists "test_Langle_tokenization"
    ("<" |> generate_token_list)
    [ Langle ]

let test_Rangle_tokenization =
  test_token_lists "test_Rangle_tokenization"
    (">" |> generate_token_list)
    [ Rangle ]

let test_LangleEqual_tokenization =
  test_token_lists "test_LangleEqual_tokenization"
    ("<=" |> generate_token_list)
    [ LangleEqual ]

let test_RangleEqual_tokenization =
  test_token_lists "test_RangleEqual_tokenization"
    (">=" |> generate_token_list)
    [ RangleEqual ]

let test_NotEqual1_tokenization =
  test_token_lists "test_NotEqual1_tokenization"
    ("<>" |> generate_token_list)
    [ NotEqual ]

let test_NotEqual2_tokenization =
  test_token_lists "test_NotEqual2_tokenization"
    ("><" |> generate_token_list)
    [ NotEqual ]

let test_Equal_tokenization =
  test_token_lists "test_Equal_tokenization"
    ("=" |> generate_token_list)
    [ Equal ]

let test_Plus_tokenization =
  test_token_lists "test_Plus_tokenization"
    ("+" |> generate_token_list)
    [ Plus ]

let test_Minus_tokenization =
  test_token_lists "test_Minus_tokenization"
    ("-" |> generate_token_list)
    [ Minus ]

let test_Mult_tokenization =
  test_token_lists "test_Mult_tokenization"
    ("*" |> generate_token_list)
    [ Mult ]

let test_Div_tokenization =
  test_token_lists "test_Div_tokenization" ("/" |> generate_token_list) [ Div ]

let () =
  let open Alcotest in
  run "Lexer_tests"
    [
      ( "test_tokenization",
        [
          test_empty_tokenization;
          QCheck_alcotest.to_alcotest test_Nat_tokenization;
          QCheck_alcotest.to_alcotest test_String_tokenization;
          QCheck_alcotest.to_alcotest test_Var_tokenization;
          test_Langle_tokenization;
          test_Rangle_tokenization;
          test_LangleEqual_tokenization;
          test_RangleEqual_tokenization;
          test_NotEqual1_tokenization;
          test_NotEqual2_tokenization;
          test_Equal_tokenization;
          test_Plus_tokenization;
          test_Minus_tokenization;
          test_Mult_tokenization;
          test_Div_tokenization;
        ] );
    ]
