(** {1 Machine language for reverse polish machine }

The reverse polish machine is a machine with a stack of integers. It
supports the following instruction.

- Push an integer on to the stack
- Execute a binary operator using the top two arguments of the stack
  and push the result on to the stack.
- Clearing the stack
- Printing the top
- Print the entire stack

An executable program for this machine is just a list of such
instructions.

 *)

(**

Ocaml variants can capture this instruction set as well. You can see
this as the AST for the target language.

@see <https://dev.realworldocaml.org/variants.html> Variants in Ocaml

*)

type program = inst list
and inst =
  | Push of int
  | Exec of Ast.binOp
  | ClearStack
  | PrintTop
  | PrintStack

(** {3 Pretty printing}

Finally, we would want to convert a "program" for the reverse polish machine
to its representation as a string. For this we use pretty printing.

 *)

module Pretty = struct

  (** pretty print an operator *)
  let operator (o : Ast.binOp) =
    match o with
    | Plus  -> "plus"
    | Minus -> "minus"
    | Mul   -> "mul"
    | Div   -> "div"

  (** pretty print an instruction *)
  let instruction (i : inst) : string  =
    match i with
    | Push x     -> String.concat " " [ "push" ; string_of_int x]
    | Exec o     -> operator o
    | ClearStack -> "clear"
    | PrintTop   -> "print"
    | PrintStack -> "stack"

  (** pretty print a program *)
  let program (p : program) = String.concat ";\n" (List.map instruction p)

end

(** {2 Simulator for the reverse polish machine}


The rest of the module defines functions for simulating the reverse
polish machine. In the first reading you may skip this as these functions
are not required by the [ec] compiler but only by the reverse polish
machine simulator [rp] which is provided with this library.

 *)

module Simulator : sig
  (** runs the given program returning the stack at the end *)
  val run : program -> int list

  (** Exception raised when the stack is not okey for the operation *)
  exception StackUnderflow of int list

end = struct
  exception StackUnderflow of int list

  let pr prompt s     = print_endline (prompt ^ " " ^ Int.to_string s)
  let printtop = function
      (x :: _) -> pr "" x
    | _        -> raise (StackUnderflow [])

  let printstack s = print_endline "^" ;
                     List.iter (pr "|") s;
                     print_endline "*"

  let step stack = function
    | Push x     -> x :: stack
    | PrintStack -> (printstack stack ; stack)
    | PrintTop   -> (printtop stack ; stack)
    | ClearStack -> []
    | Exec op    -> match stack with
                    | arg2 ::  arg1 :: rest
                      -> Ast.binOpDenote op arg1 arg2 :: rest
                    | sp
                      -> raise (StackUnderflow sp)

  let run = List.fold_left step []

end
