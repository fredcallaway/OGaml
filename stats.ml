open Yojson.Basic.Util

type t = {
  id: string;
  health: int;
  strength: int;
  speed: int;
  dexterity: int;
  magic: int;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Stats/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let health = json |> member "health" |> to_int in
  let strength = json |> member "strength" |> to_int in
  let speed = json |> member "speed" |> to_int in
  let dexterity = json |> member "dexterity" |> to_int in
  let magic = json |> member "magic" |> to_int in
  {
  id;
  health;
  strength;
  speed;
  dexterity;
  magic
  }

let to_file path game =
  failwith "TODO"

let get_health s =
  s.health


type effect = t -> t
let null_effect : effect = (fun x -> x)
let health_effect    amt (stats : t) =  {stats with health = stats.health + amt}
let strength_effect  amt (stats : t) =  {stats with strength = stats.strength + amt}
let speed_effect     amt (stats : t) =  {stats with speed = stats.speed + amt}
let dexterity_effect amt (stats : t) =  {stats with dexterity = stats.dexterity + amt}
let magic_effect     amt (stats : t) =  {stats with magic = stats.magic + amt}


