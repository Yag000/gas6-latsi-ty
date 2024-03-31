open Latsi
open Latsi.Ast

let lexbuf = Lexing.from_channel stdin

let () =
  Format.printf "Parsing...@.";
  let ast = Parser.input Lexer.lexer lexbuf in
  Format.printf "Parsed@.";
  if ast = [] then Format.printf "No program found@."
  else Format.printf "%a" pp_program ast
