%{ 
open Tiger0.Ast 
%} 

%token <int> INT
%token <string> ID
%token PLUS MINUS DIV MUL ASSIGN PRINT EOF 
%token EQUAL FOR TO DO DONE

%left PLUS MINUS
%left MUL DIV 

%start program 
%type <Tiger0.Ast.program> program

%%

program:
    list_of_statements EOF { $1 }
;
list_of_statements:
    statement { [$1] }
    | list_of_statements statement { $1 @ [$2] }
;
statement:
    ID ASSIGN expression {Assigning($1,$3)}
    | PRINT expression {Print($2)}
    | FOR ID EQUAL INT TO INT DO list_of_statements DONE {For($2,$4,$6,$8)}
;
expression:
    expression PLUS expression {Bin_Operator(Add,$1,$3)} 
    |expression MINUS expression {Bin_Operator(Sub,$1,$3)}
    |expression DIV expression {Bin_Operator(Div,$1,$3)} 
    |expression MUL expression {Bin_Operator(Mul,$1,$3)}
    |INT {Int($1)}
    |ID {Var($1)}
;

