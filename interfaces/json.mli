open Yojson.Basic.Util

(* initializes the game with the given json filename *)
val load: string -> Game.t

(* save the game state into a json file *)
val save: Game.t -> unit

(* create a new json file for a new game *)
val new_game: unit -> string

