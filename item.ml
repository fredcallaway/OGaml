
type stats = {health: int;
              strength: int;
              speed: int;
              dexterity: int;
              magic: int}

type effect = {immediate: stats -> stats;
               over_time: stats -> stats;
               ot_duration: int}


type ability = {self: effect;
                opponent: effect}

type equip_slot = 
  | Head 
  | Body
  | Legs
  | Feet
  | Hands
  | Primary
  | Secondary
  | Special 

type equip_stats = {name: string;
          description: string;
          value: int;
          ability: ability list;
          boost: stats;
          slot: equip_slot}

type consume_stats = {name: string;
            description: string;
            value: int;
            duration: int;
            action: ability;
            boost: stats}

type t = 
  | Equipable of equip_stats
  | Consumable of consume_stats

(* a set of equipped armor and weapons *)
type equip