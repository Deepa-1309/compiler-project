{ open Parser }

let digit= ['0'-'9']
let alphabet= ['a'-'z' 'A'-'Z']
let id = alphabet (alphabet | digit)*

rule read = parse
            | ['\t'  ' '  '\n'] { read lexbuf } 
            | "+" { PLUS }
            | "-" { MINUS }
            | "*" { MUL }
            | "/" { DIV }
            | ":=" { ASSIGN }
            | "=" {EQUAL}
            | "for" { FOR }
            | "to" { TO }
            | "do" { DO }
            | "done" { DONE }
            | "print" { PRINT }
            | ';' { read lexbuf }   (* ignore semicolon *)
            | digit+ as n { INT(int_of_string n)}
            | id as x {ID(x)}
            | eof { EOF }
