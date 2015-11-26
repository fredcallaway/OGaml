
type t = {opponent: Fighter.t;
          ai: AI.t;
          xp: int;
          treasure: Item.t list;
          money: int;
}

val enter_battle: t -> Player.t -> Player.t option



(* First fighter is user, second is AI, int specifies turn *)
type turn = 
  | PlayerTurn
  | AITurn
  | Win
  | Lose

type state = Fighter.t * Fighter.t * turn
