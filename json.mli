open Yojson.Basic.Util
open Player
open World


type t = {
  world : World.t;
  player : Player.t;
}

(* initializes the world and player state with the given Json object *)
val load: string -> t

(* save the current world and player state into a json file *)
val save: string -> t

