{
open Parser

exception UnkownToken of string
}

let digit = ['0'-'9']
let number = digit+
let var = ['a'-'z''A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z' '_']*
let string = '"'[' ' ',' ''' '_' ';' ':' '(' ')' '.' 'a'-'z' 'A'-'Z']*'"' 
let white = [' ' '\t']

rule lexer = parse
        | white { lexer lexbuf }
        | string as s { String(String.sub s 1 (String.length s - 2)) }
        | number as n { Nat(int_of_string n) }
        | "<=" { LangleEqual }
        | ">=" { RangleEqual }
        | "<>"|"><" { NotEqual }
        | '<' { Langle }
        | '>' { Rangle }
        | '=' { Equal }
        | '+' { Plus }
        | '-' { Minus }
        | '*' { Times }
        | '/' { Slash }
        | '(' { LParen }
        | ')' { RParen }
        | "NL" { Nl }
        | "REM" { Rem }
        | "VAVERS" { Vavers }
        | "SI" { Si }
        | "ALORS" { Alors }
        | "ENTREE" { Entree }
        | ',' { Comma }
        | "IMPRIME" { Imprime }
        | "FIN" { Fin }
        | "SOUSROUTINE" { Sousroutine }
        | "RETOURNE" { Retourne }
        | '\n' {CR}
        | eof { EOF }
        | var as v { Var(v) }
        | _ as c { raise (UnkownToken (String.make 1 c)) }
