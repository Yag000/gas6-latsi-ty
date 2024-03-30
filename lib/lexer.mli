exception UnkownToken of string

val lexer : Lexing.lexbuf -> Parser.token
