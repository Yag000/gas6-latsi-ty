open Latsi
open Latsi.Interpreter

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.input Lexer.lexer lexbuf in
  eval ast
