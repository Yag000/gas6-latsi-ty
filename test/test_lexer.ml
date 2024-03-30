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

let test_UnknownToken_exception (str : string) =
  let open Alcotest in
  test_case ("Unknown token : " ^ str) `Quick (fun () ->
      check bool "Unknown token" true
        (try
           let _ = generate_token_list str in
           false
         with
        | UnkownToken _ -> true
        | _ -> false))

let stringizer str = "\"" ^ str ^ "\""

let test_UnknownToken_string_exception (str : string) =
  test_UnknownToken_exception (stringizer str)

let test_empty_tokenization =
  test_token_lists "test_empty_tokenization" ("" |> generate_token_list) []

let test_empty_String_tokenization =
  test_token_lists "test_empty_String_tokenization"
    ("\"\"" |> generate_token_list)
    [ String "" ]

let test_String_tokenization =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Forall string x composed of custom_chars, String c is generated"
    (list arbitrary_custom_char) (fun char_list ->
      let str = join_char_list char_list in
      let token_list = generate_token_list ("\"" ^ str ^ "\"") in
      [ String str ] = token_list)

let test_Var_tokenization =
  let open QCheck in
  Test.make ~count:1000 ~name:"Forall x in [A-Z], Var x is generated"
    arbitrary_var (fun c -> [ Var c ] = (Char.escaped c |> generate_token_list))

let test_Nat_tokenization =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Forall strictly positive integers x, the token `Nat x` is generated"
    pos_int (fun n -> n |> string_of_int |> generate_token_list = [ Nat n ])

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

let test_mix_tokenization =
  test_token_lists "test_mix_tokenization"
    ("\",'abc\" \"_;de:f\" \"(GHI).\" 123 V <= >= <> >< < + > - * /"
     ^ "\",'abc\"\"_;de:f\"\"(GHI).\"123V<=>=<>><<+>-*/"
    |> generate_token_list)
    [
      String ",'abc";
      String "_;de:f";
      String "(GHI).";
      Nat 123;
      Var 'V';
      LangleEqual;
      RangleEqual;
      NotEqual;
      NotEqual;
      Langle;
      Plus;
      Rangle;
      Minus;
      Mult;
      Div;
      String ",'abc";
      String "_;de:f";
      String "(GHI).";
      Nat 123;
      Var 'V';
      LangleEqual;
      RangleEqual;
      NotEqual;
      NotEqual;
      Langle;
      Plus;
      Rangle;
      Minus;
      Mult;
      Div;
    ]

let test_invalid_number_float =
  let open QCheck2 in
  Test.make ~count:1000 ~name:"lexer raises UnknownToken for floats"
    ~print:string_of_float Gen.float (fun i ->
      try
        let _ = i |> string_of_float |> generate_token_list in
        false
      with _ -> true)

let test_illegal_inner_string_characters =
  List.map
    (fun s -> test_UnknownToken_string_exception s)
    [ "é"; "è"; "à"; "ù"; "&"; "/"; "-"; "|"; "?"; "!" ]

let test_illegal_characters =
  List.map
    (fun s -> test_UnknownToken_exception s)
    [ "é"; "è"; "à"; "ù"; "&"; "|"; "?"; "!" ]

let test_numbers_inner_string = test_UnknownToken_string_exception "1 2 3 4"

let () =
  let open Alcotest in
  run "Lexer tests"
    [
      ( "test tokenization",
        [
          QCheck_alcotest.to_alcotest test_Nat_tokenization;
          test_empty_tokenization;
          test_empty_String_tokenization;
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
          test_mix_tokenization;
        ] );
      ( "test_illegal_tokenization",
        [
          QCheck_alcotest.to_alcotest test_invalid_number_float;
          test_numbers_inner_string;
        ]
        @ test_illegal_characters @ test_illegal_inner_string_characters );
    ]
