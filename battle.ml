type ai


type t = {opponent: Fighter.t;
          ai: ai;
          xp: int;
          treasure: Item.t list;
          money: int;
}


(* The player whose turn it is is first. *)
type state = Fighter.t * Fighter.t


type effect = Fighter.t -> Fighter.t
let null_effect : effect = (fun x -> x)

let get_effects item : effect * effect = 
  null_effect, null_effect

let apply_effects (f1, f2) (e1, e2) : state = 
  (e1 f1), (e2 f2)


type action = 
  | Consume of Item.t
  | Use of Item.t

let get_user_action state : action = failwith "unimplemented"
let get_ai_action state : action = failwith "unimplemented"

let do_action (state: state) (act: action) : state =
    match act with 
    | Consume item -> begin
      let f1, f2 = state in
      let state' = Fighter.remove_item f1 item, f2 in
      apply_effects state' (get_effects item)
    end
    | Use item -> begin
      apply_effects state (get_effects item)
    end


type turn = 
  | PlayerTurn
  | AITurn
  | Win
  | Lose

let rec loop (turn, state) : bool * state = 
  let switch turn (a,b) = (not turn), (b,a) in
  let action = (if turn then get_user_action else get_ai_action) state in
  let state = do_action state action in
  if Fighter.is_dead (snd state) 
  then turn, state
  else loop (switch turn state)


let give_booty battle player : Player.t =
  failwith "unimplemented"

let enter_battle battle (player) =
  let player_fighter = Fighter.make player.Player.stats 
                                    player.Player.inventory 
                                    player.Player.equipped in
  match loop (true, (player_fighter, battle.opponent)) with
  | (true, (pf, _)) -> Some (give_booty {player with Player.inventory = pf.Fighter.inventory})
  | (false, (_, _)) -> None
