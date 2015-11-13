open Battle
open Shop
open Player

type t = {
  battles : Battle.t list;
  shop : Shop.t;
}

(* enter the specified zone with the player *)
(* return:
 * the new player and true if the player unlocks the next zone
 * the new player and false if the player did not unlock the next zone
 *)
val enter_zone: t -> Player.t -> (Player.t * bool)

(* return: state with zone unlocked *)
val unlock_zone: t -> Zone.t -> t