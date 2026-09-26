%token <int> CONST
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LPAR
%token RPAR
%token EOF
%token NEWLINE
%start <Ast.expr list> prog
%left PLUS MINUS
%left MUL DIV
%%

prog:
  | es=exps EOF {es}
;
exps:
  | es=separated_list(NEWLINE, expr) { es }
;
expr:
  | e1=expr PLUS  e2=expr { Ast.plus  e1 e2 }
  | e1=expr MINUS e2=expr { Ast.minus e1 e2 }
  | e1=expr MUL   e2=expr { Ast.mul   e1 e2 }
  | e1=expr DIV   e2=expr { Ast.div   e1 e2 }
  | LPAR e=expr RPAR {e}
  | c=CONST               { Ast.Const c     }
;
