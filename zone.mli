open Battle
open Shop
open Player

type t = {
  battles : Battle.t list;
  shop : Shop.t;
}

val enter_zone : (t * Player.t) -> (t * Player.t)