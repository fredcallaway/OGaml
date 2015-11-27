type stats = {health: int;
              strength: int;
              speed: int;
              dexterity: int;
              magic: int}


type effect = stats -> stats
let null_effect : effect = (fun x -> x)
let health_effect    amt (stats : stats) =  {stats with health = stats.health + amt}
let strength_effect  amt (stats : stats) =  {stats with strength = stats.strength + amt}
let speed_effect     amt (stats : stats) =  {stats with speed = stats.speed + amt}
let dexterity_effect amt (stats : stats) =  {stats with dexterity = stats.dexterity + amt}
let magic_effect     amt (stats : stats) =  {stats with magic = stats.magic + amt}


