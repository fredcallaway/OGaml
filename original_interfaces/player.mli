
type t = {stats: Item.stats;
          inventory: Item.t list;
          equipped: Item.equip;
          money: int;
          expereience: int;
          level: int}

(* must keep track of level up schedule.. maybe in state? *)
