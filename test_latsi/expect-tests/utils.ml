open Latsi.Interpreter

let eval_str program =
  let lexbuf = Lexing.from_string program in
  let ast = Latsi.Parser.input Latsi.Lexer.lexer lexbuf in
  eval ast
