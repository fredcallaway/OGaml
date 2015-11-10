open Player
open Item


type t = {opponent: Player.t;
          xp: int;
          treasure: Item.t list;
          money: int
}


val enter_battle: (t * Player.t) -> Player.t option


module type Fighter = sig
  type abil
  type item
  type player

  type t = {player: player;
            hp: float;
            effects: effect list}
  (* an effect alters a fighter for a number of turns *)
  and effect = t -> t

  type state = t * t  (* the fighter whose turn it is is the first element *)
  type action = Item of item | Abil of abil

  (* given your state and your oponents state, picks an action*)
  val choose_action: state -> action

  (* applies an action to a state *)
  val do_action: state -> action -> state
end
