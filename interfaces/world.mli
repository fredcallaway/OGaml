
(* the full game state *)
type t = {
  zones : Zone.t list;
  player : Player.t;
}

(* initializes the world state with the given Json object *)
val init_world: Json.t -> Player.t -> t

(* enter the world with the player *)
(* return:
 * the new player and true if the player completes the game
 * the new player and false if the player did not complete the game
 *)
val enter_world: t -> Player.t -> (Player.t * bool)
