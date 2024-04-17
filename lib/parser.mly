%{
open Ast
%}


%token <string> String
%token <int> Nat
%token <char> Var
%token Langle
%token Rangle
%token LangleEqual
%token RangleEqual
%token NotEqual
%token Equal
%token Plus
%token Minus
%token Times
%token Slash
%token LParen
%token RParen
%token Rem
%token Vavers
%token Entree
%token Imprime
%token Comma
%token Fin
%token Nl
%token CR
%token EOF


%start<Ast.program> input

%left Plus Minus
%left Times Slash


%%

input: c=program EOF { c }


program:
    l=line* { l }

line:
    n=Nat i=instr CR { 
        {number = n; instr = i} }

instr:
    v=var Equal e=expression { Assign (v,e)}
    | Rem s=String { Rem s }
    | Vavers e=expression { Vavers e }
    | Entree l= separated_nonempty_list(Comma, var) { Entree l }
    | Imprime el=separated_nonempty_list(Comma, expr) { Imprime el }
    | Fin { Fin}
    | Nl { Nl }

expr:
    s=String {String_ s} 
    | e=expression {Expression e}

expression:
    Plus x=expression { Unop ( Pos, x) }
    | Minus x=expression { Unop ( Neg, x) }
    | x=expression Plus y=expression { Binop ( Add, x, y) }
    | x=expression Minus y=expression { Binop ( Sub, x, y) }
    | x=expression Times y=expression { Binop ( Mul, x, y) }
    | x=expression Slash y=expression  { Binop ( Div, x, y) }
    | n=Nat {Number n}
    | v=var {Var v}
    | LParen e=expression RParen {e}

var:
    x=Var { x }

