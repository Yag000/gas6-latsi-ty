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
%token CR
%token EOF


%start<Ast.program> input

%%

input: c=program EOF { c }


program:
    l=line* CR { l }

line:
    n=Nat i=instr { {number = n; instr = i} }
    | n=Nat Rem s=String { {number = n; instr = Rem s} }

instr:
    v=var Equal e=expression {Assing (v,e)}

expression:
    x=option(term) Plus y=term {Add (x,y)}
    | x=option(term) Minus y=term {Sub (x,y)}


term:
    x=factor Times y=factor {Mul (x,y)}
    | x=factor Slash y=factor {Ast.Div (x,y)}

factor:
    n=Nat {Number n}
    | v=var {Var v}
    | LParen e=expression RParen {e}


var:
    x=Var { x }

