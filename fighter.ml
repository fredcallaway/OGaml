
type t = {stats: Item.stats;
          inventory: Item.t list;
          equipped: Item.equip;
          hp: float;
          effects: effect list}
(* an effect alters a fighter for a number of turns *)
and effect = t -> t


let make (stats: Item.stats) (items: Item.t list) (equip: Item.equip) : t = 
  failwith "unimplemented"

let remove_item f item : t = failwith "unimplemented"

let is_dead f : bool = failwith "unimplemented"