open Latsi_extension
open Latsi_extension.Interpreter

let () =
  let input_file = Sys.argv.(1) in
  let input_channel = open_in input_file in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.input Lexer.lexer lexbuf in
  eval ast
