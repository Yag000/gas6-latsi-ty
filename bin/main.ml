open Latsi
open Latsi.Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.input Lexer.lexer lexbuf in
  Format.printf "%a" pp_program ast
