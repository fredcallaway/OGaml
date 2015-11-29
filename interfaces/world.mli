type t = {
  id : string;
  completed : bool;
  zones : Zone.t list;
}

(* enter the world with the player *)
(* postcondition: the new player and the updated world on exit *)
val enter_world: t -> Player.t -> (t * Player.t)

(* precondition: all zones in world must have unique ids *)
(* postcondition: world with updated zone and next zone unlocked if zone was completed*)
val update_zones: t -> Zone.t -> t
