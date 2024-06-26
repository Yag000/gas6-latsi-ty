open Latsi.Lexer
open Latsi.Parser
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
    ~name:
      "Forall string x composed of custom string characters, the token `String \
       x` is generated" (list arbitrary_custom_char) (fun char_list ->
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
    [ Times ]

let test_Div_tokenization =
  test_token_lists "test_Div_tokenization"
    ("/" |> generate_token_list)
    [ Slash ]

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
      Times;
      Slash;
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
      Times;
      Slash;
    ]

let test_lparen_tokenization =
  test_token_lists "test_lparen_tokenization"
    ("(" |> generate_token_list)
    [ LParen ]

let test_rparen_tokenization =
  test_token_lists "test_rparen_tokenization"
    (")" |> generate_token_list)
    [ RParen ]

let test_parens_tokenization =
  test_token_lists "test_parens_tokenization"
    ("()" |> generate_token_list)
    [ LParen; RParen ]

let test_cr_tokenization =
  test_token_lists "test_cr_tokenization" ("\n" |> generate_token_list) [ CR ]

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

let test_rem_tokenization =
  [
    test_token_lists "test_rem_tokenization"
      ("REM" |> generate_token_list)
      [ Rem ];
    test_UnknownToken_exception "rem";
    test_UnknownToken_exception "Rem";
  ]

let test_vavers_tokenization =
  [
    test_token_lists "test_vavers_tokenization"
      ("VAVERS" |> generate_token_list)
      [ Vavers ];
    test_UnknownToken_exception "vavers";
    test_UnknownToken_exception "Vavers";
  ]

let test_entree_tokenization =
  [
    test_token_lists "test_entree_tokenization"
      ("ENTREE" |> generate_token_list)
      [ Entree ];
    test_UnknownToken_exception "entree";
    test_UnknownToken_exception "Entree";
  ]

let test_comma_tokenization =
  [
    test_token_lists "test_comma_tokenization"
      ("," |> generate_token_list)
      [ Comma ];
    test_UnknownToken_exception "comma";
    test_UnknownToken_exception "Comma";
  ]

let test_imprime_tokenization =
  [
    test_token_lists "test_imprime_tokenization"
      ("IMPRIME" |> generate_token_list)
      [ Imprime ];
    test_UnknownToken_exception "imprime";
    test_UnknownToken_exception "Imprime";
  ]

let test_fin_tokenization =
  [
    test_token_lists "test_fin_tokenization"
      ("FIN" |> generate_token_list)
      [ Fin ];
    test_UnknownToken_exception "fin";
    test_UnknownToken_exception "Fin";
  ]

let test_nl_tokenization =
  [
    test_token_lists "test_NL_tokenization" ("NL" |> generate_token_list) [ Nl ];
    test_UnknownToken_exception "Nl";
    test_UnknownToken_exception "nL";
    test_UnknownToken_exception "nl";
  ]

let test_si_tokenization =
  [
    test_token_lists "test_si_tokenization" ("SI" |> generate_token_list) [ Si ];
    test_UnknownToken_exception "si";
    test_UnknownToken_exception "Si";
  ]

let test_alors_tokenization =
  [
    test_token_lists "test_alors_tokenization"
      ("ALORS" |> generate_token_list)
      [ Alors ];
    test_UnknownToken_exception "alors";
    test_UnknownToken_exception "Alors";
  ]

let () =
  let open Alcotest in
  run "Lexer_tests"
    [
      ( "test_tokenization",
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
          test_lparen_tokenization;
          test_rparen_tokenization;
          test_parens_tokenization;
          test_cr_tokenization;
        ]
        @ test_rem_tokenization @ test_vavers_tokenization
        @ test_entree_tokenization @ test_comma_tokenization
        @ test_imprime_tokenization @ test_fin_tokenization
        @ test_nl_tokenization @ test_si_tokenization @ test_alors_tokenization
      );
      ( "test_illegal_tokenization",
        [
          QCheck_alcotest.to_alcotest test_invalid_number_float;
          test_numbers_inner_string;
        ]
        @ test_illegal_characters @ test_illegal_inner_string_characters );
    ]
