%{
open Ast

%}


%token <string> String
%token <int> Nat
%token <string> Var
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
%token Si
%token Alors
%token Entree
%token Imprime
%token Sousroutine 
%token Retourne
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

assign:
    p=separated_pair(Var, Equal, expression) { p }

split_assign:
    vh=Var Comma vt=separated_nonempty_list(Comma, Var) Equal eh=expression Comma et=separated_nonempty_list(Comma, expression) { 
            let vars = vh::vt in
            let expressions = eh::et in
            if List.compare_lengths vars expressions = 0 
            then
                SplitAssign (vars, expressions)
            else
                raise ParserError
        }

instr:
    | l=separated_nonempty_list(Comma, assign) { Assign l }
    | sa=split_assign { sa }
    | Rem s=String { Rem s }
    | Vavers e=expression { Vavers e }
    | Si e1=expression r=relop e2=expression Alors i=instr{ SiAlors (r, e1, e2, i) }
    | Entree l= separated_nonempty_list(Comma, var) { Entree l }
    | Imprime el=separated_nonempty_list(Comma, expr) { Imprime el }
    | Sousroutine e=expression { Sousroutine e }
    | Retourne { Retourne }
    | Fin { Fin }
    | Nl { Nl }

relop:
    Langle { Lt }
    | Rangle { Gt }
    | LangleEqual { Le }
    | RangleEqual { Ge }
    | NotEqual { Ne }
    | Equal { Eq }

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

