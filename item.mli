type abil
type stats

type equip_stats = {name: string;
					description: string;
					value: string;
					ability: abil list;
					boost: stats;
					(* eq_slot: } *)}

type consume_stats = {name: string;
					  description: string;
					  value: string}


(*
 *
 *
 *)
type item = 
| Equipable of equip_stats
| Consumable of consume_stats