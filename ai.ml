

(* naive AI, uses whatever item is first in its equipped list. *)
let get_ai_action state : Battle.action =
  let ai = (snd state) in
  let ai_equipped = Fighter.get_equipped ai in
  (* printf "Opponent used %s!\n" (Item.get_id (List.hd ai_equipped)); *)
  Use (List.hd ai_equipped)