type abil
type item
open Stats

type equip = {head: item;
				body: item;
				legs: item;
				feet: item;
				special: item;
				weapon: item}

type t = {stats: Stats.t;
          abilities: abil list;
          inventory: item list;
          equipped: equip
          }
