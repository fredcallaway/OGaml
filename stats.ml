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

let combine (st1: t) (st2: t) : t =
  {
  id = st2.id;
  health = if (st1.health + st2.health) < 0 then 0 else st1.health + st2.health;
  strength = st1.strength + st2.strength;
  speed = st1.speed + st2.speed;
  dexterity = st1.dexterity + st2.dexterity;
  magic = st1.magic + st2.magic
  }

let get_stats_list (st: t): string list =
  ["health: " ^ string_of_int st.health; "strength: " ^ string_of_int st.strength;
  "speed: " ^ string_of_int st.speed; "dexterity: " ^ string_of_int st.dexterity;
  "magic: " ^ string_of_int st.magic]

let print_battle_stats (ustats: t) (ostats: t) =
  let user_stats = get_stats_list ustats in
  let opp_stats = get_stats_list ostats in
  let f str1 str2 = printf "\t%s\t\t\t\t\t%s\n" str1 str2 in
  printf "\nUser Stats:\t\t\t\t\tOpponent Stats:\n";
  List.iter2 f user_stats opp_stats

type effect = t -> t
let null_effect : effect = (fun x -> x)
let health_effect    amt (stats : t) =  {stats with health = stats.health + amt}
let strength_effect  amt (stats : t) =  {stats with strength = stats.strength + amt}
let speed_effect     amt (stats : t) =  {stats with speed = stats.speed + amt}
let dexterity_effect amt (stats : t) =  {stats with dexterity = stats.dexterity + amt}
let magic_effect     amt (stats : t) =  {stats with magic = stats.magic + amt}


