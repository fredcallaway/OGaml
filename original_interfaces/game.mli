type t = {
  id : string;
  world : World.t;
  player : Player.t;
}

(* start the game *)
val enter_game: unit -> unit