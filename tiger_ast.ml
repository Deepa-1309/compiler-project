(* ast.ml --> this file is for defining grammar in ocaml --> ASt *)
type operations = Add
                | Sub
                | Mul
                | Div
type expression = Int of int
                | Var of string
                | Bin_Operator of operations * expression * expression

(* defining statements*)
type stmt = Assigning of string * expression
          | Print of expression
          | For of string * int * int * stmt list   (*for loops*)

(*  given program is a list of statements *)
type program = stmt list 

