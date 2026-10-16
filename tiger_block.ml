open Ir
module type INST = sig
  type t
  val isJumpLike : t -> bool
  val isTarget   : t -> bool
end  

module InstrInst = struct
  type t = instr        

  let isJumpLike = function
    | Jump _ | Cond_jump _ -> true
    | _ -> false

  let isTarget = function
    | Label _ -> true
    | _ -> false
end

module BasicBlocks (I : INST) = struct
  type block = I.t list

  let basicBlocks ins =
  let add_block current blocks =
    if current = [] then blocks
    else current :: blocks
  in

  let (current, blocks) =
    List.fold_right
      (fun inst (current, blocks) ->

        if I.isJumpLike inst then
          let blocks = add_block current blocks in
          ([inst], blocks)

        else if I.isTarget inst then
          (* In fold_right, current already has the NEXT block's instructions *)
          (* So prepend label directly to current, then save as a block *)
          let new_current = inst :: current in
          let blocks = add_block new_current blocks in
          ([], blocks)          (* ← start fresh for instructions BEFORE label *)

        else
          (inst :: current, blocks)

      )
      ins
      ([], [])
  in

  add_block current blocks
end
