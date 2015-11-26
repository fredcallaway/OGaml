
type t = {stats: Item.stats;
          inventory: Item.t list;
          equipped: Item.equip;
          hp: float;
          effects: effect list}
(* an effect alters a fighter for a number of turns *)
and effect = t -> t


val make :  Item.stats -> Item.t list -> Item.equip -> t  


