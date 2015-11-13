type item 



type t = {stats: stats;
          inventory: item list;
          equipped: item list;
          money: int;
          expereience: int;
          level: int}

(* must keep track of level up schedule.. maybe in state? *)

val get_abilities: unit -> ability list

val level_up: unit -> t



