type abil
type item


type stats = {health: float;
              attack: float}

(* type equip = {head: item;
				body: item;
				legs: item;
				feet: item;
				special: item;
				weapon: item} *)

type t = {stats: stats;
          abilities: abil list;
          inventory: item list;
          (* equipped: equip *)
          }

