open Ast
module type TEMP = sig 
        type temp
        val newtemp : unit -> temp
        val tempToString : temp -> string

        type label
        val newlabel : unit -> label
        val labelToString : label -> string
        
end

module Temp : TEMP = struct 
        type temp = int
        let nextTemp = ref 0
        (*fun newtemp _ =*)
        let newtemp () = 
                let t = !nextTemp in
                nextTemp := !nextTemp +1;
                t 
        (* fun tempToString t =*)
        let tempToString t = "t" ^ string_of_int t 

        type label = int
        let labCount = ref 0
        let newlabel () =
                let l = !labCount in
                labCount := !labCount +1;
                l 
        let labelToString l = "L" ^ string_of_int l
        
end 

type instr=
  | Move of Temp.temp * Temp.temp
  | Move_Int of Temp.temp * int
  | Load of Temp.temp * string
  | Store of string * Temp.temp
  | Operator of operations * Temp.temp * Temp.temp * Temp.temp
  | PrintT of Temp.temp
  (*for loops*)
  | Label of Temp.label 
  | Jump of Temp.label 
  | Cond_jump of Temp.temp * Temp.temp * Temp.label 
type program_ir = instr list

let string_of_op = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"

let string_of_instr = function
  | Move (t1, t2) ->
      Temp.tempToString t1 ^ " = " ^ Temp.tempToString t2

  | Move_Int (t, i) ->
      Temp.tempToString t ^ " = " ^ string_of_int i

  | Load (t, s) ->
      Temp.tempToString t ^ " = " ^ s

  | Store (s, t) ->
      s ^ " = " ^ Temp.tempToString t

  | Operator (op, dst, t1, t2) ->
      Temp.tempToString dst ^ " = " ^
      Temp.tempToString t1 ^ " " ^
      string_of_op op ^ " " ^
      Temp.tempToString t2

  | PrintT t ->
      "print " ^ Temp.tempToString t

  | Label l ->
      Temp.labelToString l ^ ":"

  | Jump l ->
      "jump " ^ Temp.labelToString l

  | Cond_jump (t1, t2, l) ->
      "if " ^
      Temp.tempToString t1 ^ " > " ^
      Temp.tempToString t2 ^ " goto " ^
      Temp.labelToString l
