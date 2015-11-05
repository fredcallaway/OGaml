type player
type item
type abil

type t = {opponent: player;
          xp: int;
          treasure: item list;
          money: int}

type outcome = 
  | Loss
  | Victory of player

val fight: t -> player -> outcome