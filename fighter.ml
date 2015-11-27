open Stats

type fighter = {stats: stats;
                equipped: Item.equip}

let alive f : bool = f.stats.health > 0

let remove_item f item : fighter = failwith "unimplemented"

let apply_effect effect (f : fighter) = 
  {f with stats = (effect f.stats)}