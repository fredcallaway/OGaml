type t = {
  id : string;
  unlocked : bool;
  completed : bool;
  battles : Battle.t list;
  shop : Shop.t;
}

(* enter the zone with the player *)
(* postcondition: the new player and the updated zone on exit *)
val enter_zone: t -> Player.t -> (t * Player.t)

(* precondition: all battles in zone must have unique ids *)
(* postcondition: zone with updated battle and next battle unlocked if battle was completed*)
val update_battles: t -> Battle.t -> t
