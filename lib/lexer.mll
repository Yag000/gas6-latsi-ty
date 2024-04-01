{
open Token

exception UnkownToken of string
}

let digit = ['0'-'9']
let number = digit+
let var = ['A'-'Z']
let string = '"'[' ' ',' ''' '_' ';' ':' '(' ')' '.' 'a'-'z''A'-'Z']*'"'
let white = [' ' '\t']

rule lexer = parse
        | white { lexer lexbuf }
        | string as s { String(String.sub s 1 (String.length s - 2)) }
        | number as n { Nat(int_of_string n) }
        | var as v { Var(v) }
        | "<=" { LangleEqual }
        | ">=" { RangleEqual }
        | "<>"|"><" { NotEqual }
        | '<' { Langle }
        | '>' { Rangle }
        | '=' { Equal }
        | '+' { Plus }
        | '-' { Minus }
        | '*' { Mult }
        | '/' { Div }
        | eof { EOF }
        | _ as c { raise (UnkownToken (String.make 1 c)) }
