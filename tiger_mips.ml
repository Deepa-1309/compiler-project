open Ir
open Backend_mips

module TempMap = Map.Make(struct
  type t = Temp.temp
  let compare = compare
end)

(* available registers *)
let regs =
  [Reg.T0; Reg.T1; Reg.T2; Reg.T3; Reg.T4;
   Reg.T5; Reg.T6; Reg.T7; Reg.T8; Reg.T9;
   Reg.S0; Reg.S1; Reg.S2; Reg.S3; Reg.S4;
   Reg.S5; Reg.S6; Reg.S7]

(* map temporaries to MIPS registers *)
let temp_to_reg t regmap =
  if TempMap.mem t regmap then
    (TempMap.find t regmap, regmap)
  else
    let reg =
      try List.nth regs (TempMap.cardinal regmap)
      with _ -> failwith "Out of registers"
    in
    let newmap = TempMap.add t reg regmap in
    (reg, newmap)

(* translate IR instruction to MIPS instruction *)
let ir_instr_to_mips instr regmap =
  match instr with

  | Move_Int (t,n) -> 
      let (r,regmap) = temp_to_reg t regmap in
      ([Inst.Li (r,n)], regmap)

  | Move (t1,t2) ->
      let (r1,regmap) = temp_to_reg t1 regmap in
      let (r2,regmap) = temp_to_reg t2 regmap in
      ([Inst.Add (r1,r2,Reg.Zero)], regmap)

  | Load (t,_) ->
      let (r,regmap) = temp_to_reg t regmap in
      ([Inst.Lw (r,0,Reg.Gp)], regmap)

  | Store (_,t) ->
        let (r,regmap) = temp_to_reg t regmap in
        ([Inst.Sw (r,0,Reg.Gp)], regmap)
    
  | Operator (op,t,t1,t2) ->
        let (r,regmap) = temp_to_reg t regmap in
        let (r1,regmap) = temp_to_reg t1 regmap in
        let (r2,regmap) = temp_to_reg t2 regmap in
        (
        match op with
        | Add -> ([Inst.Add (r,r1,r2)], regmap)
        | Sub -> ([Inst.Sub (r,r1,r2)], regmap)
        | Mul -> ([Inst.Mul (r,r1,r2)], regmap)
        | Div -> ([Inst.Div (r1,r2); Inst.Mflo r], regmap)
        )
  | PrintT t ->
        let (r,regmap) = temp_to_reg t regmap in
        ([
          (* print integer *)
          Inst.Add (Reg.A0,r,Reg.Zero);
          Inst.Li (Reg.V0,1);
          Inst.Syscall;
    
          (* print newline *)
          Inst.Li (Reg.A0,10);   (* '\n' *)
          Inst.Li (Reg.V0,11);   (* print char *)
          Inst.Syscall;
        ], regmap)

  (* for loop *)
  | Label l ->
      ([Inst.Label (Temp.labelToString l)], regmap)

  | Jump l ->
      ([Inst.Jmp (Temp.labelToString l)], regmap)

  | Cond_jump (t,n,l) ->
      let (r1,regmap) = temp_to_reg t regmap in
      let (r2,regmap) = temp_to_reg n regmap in
      ([
        Inst.Bgt (r1,r2,Temp.labelToString l)
      ], regmap)

(* convert entire IR program *)
let ir_program_to_mips prog =
  let rec helper prog regmap =
    match prog with
    | [] -> ([],regmap)
    | i::rest ->
        let (insts,regmap) = ir_instr_to_mips i regmap in
        let (rest_insts,regmap) = helper rest regmap in
        (insts @ rest_insts, regmap)
  in
  fst (helper prog TempMap.empty)

(* print program using Lab3 pretty printer *)
let print_mips prog =
  let mips_prog = ir_program_to_mips prog in
  let mips_prog =
    [Inst.Label "main"] @ mips_prog @
    [Inst.Li (Reg.V0,10); Inst.Syscall]   (* exit *)
  in
  print_string
    (Backend_mips.Mips.pretty_program
        (fun x -> x)
        Backend_mips.Reg.pretty
        mips_prog)
