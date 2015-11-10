type abil
type stats

type equip_stats = {name: string;
					description: string;
					value: int;
					ability: abil list;
					boost: stats;
					(* eq_slot: } *)}

type consume_stats = {name: string;
					  description: string;
					  value: int}


(*
 *
 *
 *)
type item = 
| Equipable of equip_stats
| Consumable of consume_stats