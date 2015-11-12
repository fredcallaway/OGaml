type abil

type equip_stats = {name: string;
					description: string;
					value: int;
					ability: abil list;
					boost: Stats.t;
					(* eq_slot: } *)}

type consume_stats = {name: string;
					  description: string;
					  value: int}


(* let add_health n stats = {stats with health = stats.health + n} *)

(*
 *
 *
 *)
type item = 
| Equipable of equip_stats
| Consumable of consume_stats
