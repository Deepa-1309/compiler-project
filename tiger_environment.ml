open Ir 

module Env = Map.Make(String)

type t = Temp.temp Env.t

let empty = Env.empty

let lookup env x =
  Env.find x env

let add x t env = Env.add x t env
