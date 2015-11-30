open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  health: int;
  strength: int;
  speed: int;
  dexterity: int;
  magic: int;
}

let from_json id json =
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

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Stats/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  from_json id json

let to_file path stats =
  failwith "TODO"

let get_health s =
  s.health

let get_stats_list (st: t): string list =
  ["health: " ^ string_of_int st.health; "strength: " ^ string_of_int st.strength;
  "speed: " ^ string_of_int st.speed; "dexterity: " ^ string_of_int st.dexterity;
  "magic: " ^ string_of_int st.magic]

let print_battle_stats (ustats: t) (ostats: t) =
  let user_stats = get_stats_list ustats in
  let opp_stats = get_stats_list ostats in
  let f str1 str2 = printf "%s\t\t\t\t\t%s\n" str1 str2 in
  List.iter2 f user_stats opp_stats

type effect = t -> t
let null_effect : effect = (fun x -> x)
let health_effect    amt (stats : t) =  {stats with health = stats.health + amt}
let strength_effect  amt (stats : t) =  {stats with strength = stats.strength + amt}
let speed_effect     amt (stats : t) =  {stats with speed = stats.speed + amt}
let dexterity_effect amt (stats : t) =  {stats with dexterity = stats.dexterity + amt}
let magic_effect     amt (stats : t) =  {stats with magic = stats.magic + amt}


