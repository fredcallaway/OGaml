open Fighter
type ai


type t = {opponent: fighter;
          ai: ai; (* for now, we ignore this parameter *)
          xp: int;
          treasure: Item.t list;
          money: int;
}


(* The player whose turn it is is first. *)
type state = fighter * fighter


type action = 
  | Consume of Item.t
  | Use of Item.t

let get_user_action state : action = failwith "unimplemented"
let get_ai_action state : action = failwith "unimplemented"

let apply_effects (e1, e2) (f1, f2) : state = 
  apply_effect e1 f1, apply_effect e2 f2

let do_action (state: state) (act: action) : state =
    match act with 
    | Consume item -> begin
      let f1, f2 = state in
      let state' = remove_item f1 item, f2 in
      apply_effects (Item.get_effects item) state'
    end
    | Use item -> begin
      apply_effects (Item.get_effects item) state
    end

(* main loop of battle, called once for each turn *)
let rec loop (turn, state) : bool * state = 
  (* turn being true means it is players turn *)
  let switch turn (a,b) = (not turn), (b,a) in
  let action = (if turn then get_user_action else get_ai_action) state in
  let state = do_action state action in
  let f1, f2 = state in
  match alive f1, alive f2 with
  | true, true -> loop (switch turn state)
  | true, false -> turn, state
  | false, true -> switch turn state
  | false, false -> failwith "????"

(* Give the player the reward, and update the players inventory *)
let clean_up player fighter battle : Player.t =
  (* TODO *)
  player

let enter_battle battle (player) =
  match loop (true, (player.Player.fighter, battle.opponent)) with
  | (true, (fighter, _)) -> Some (clean_up player fighter battle)
  | (false, (_, _)) -> None
