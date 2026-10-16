open Ast
open Ir
open Environment

(*converts expressions → IR instructions*)
let rec translate env = function
  | Int n ->
  
      let t = Temp.newtemp () in
      (t, [Move_Int(t,n)])

  | Var x ->
  
      let t = lookup env x in
      (t, [])
      
  | Bin_Operator(op,e1,e2) ->
      let (t1,c1) = translate env e1 in
      let (t2,c2) = translate env e2 in
      let t = Temp.newtemp () in
      (t, c1 @ c2 @ [Operator(op,t,t1,t2)])

let rec codeGenList env xs =
  match xs with
  | [] -> (env, [])
  | s::rest ->
      let (env2,c1) = codeGen env s in
      let (env3,c2) = codeGenList env2 rest in
      (env3, c1 @ c2)

and codeGen env = function
  | Assigning(var, e) ->
      let (t, code) = translate env e in
      let new_env = Env.add var t env in   (* ← reuse t directly *)
      (new_env, code)                       (* ← no extra Move needed *)

  | Print e ->
      let (t,code) = translate env e in
      (env, code @ [PrintT t])

  | For(ident,start,stop,body) ->
      forLoop env ident start stop body

and forLoop env ident start stop body =
  let t = Temp.newtemp () in
  let lstart = Temp.newlabel () in
  let lend = Temp.newlabel () in
  let etap = Env.add ident t env in
  let init = [Move_Int(t,start)] in
  let tstop = Temp.newtemp () in
  let cond = [Move_Int(tstop,stop); Cond_jump(t,tstop,lend)] in
  let (_,body_code) = codeGenList etap body in
  let one = Temp.newtemp () in
  let incr = [Move_Int(one,1); Operator(Add,t,t,one)] in
  let out_env = env in
  let code =
    init @ [Label lstart] @ cond
    @ body_code @ incr
    @ [Jump lstart] @ [Label lend]
  in
  (out_env, code)
