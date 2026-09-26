(** {1 Compilation }

    The {e source program} is a list of expression given in separate
    lines. The compiler is essentially the function {!val:compile}
    given below. It makes use of the function {!val:compileExpr}.

 *)

(** Compile a single expression *)
let rec compileExpr e =
  match e with
  | Ast.Const c -> [Machine.Push c]
  | Ast.Op (e1, o, e2) ->
     compileExpr e1 @ compileExpr e2 @ [Machine.Exec o]

(** Compile a list of expression *)
let rec compile es =
  match es with
  | [] -> []
  | (x :: xs) -> compileExpr x @ [Machine.PrintTop] @ compile xs
