open Latsi_extension.Interpreter

let eval_str program =
  let lexbuf = Lexing.from_string program in
  let ast = Latsi_extension.Parser.input Latsi_extension.Lexer.lexer lexbuf in
  eval ast
