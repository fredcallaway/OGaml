open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  health: float;
  strength: float;
  speed: float;
  dexterity: float;
  magic: float;
}

let from_json id json =
  let health = json |> member "health" |> to_float in
  let strength = json |> member "strength" |> to_float in
  let speed = json |> member "speed" |> to_float in
  let dexterity = json |> member "dexterity" |> to_float in
  let magic = json |> member "magic" |> to_float in
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

let to_json stats =
  let health_json = `Float (stats.health) in
  let strength_json = `Float (stats.strength) in
  let speed_json = `Float (stats.speed) in
  let dexterity_json = `Float (stats.dexterity) in
  let magic_json = `Float (stats.magic) in
  `Assoc [
    ("health", health_json);
    ("strength", strength_json);
    ("speed", speed_json);
    ("dexterity", dexterity_json);
    ("magic", magic_json)
  ]

let to_file path stats =
  let stats_json = to_json stats in
  let filename = stats.id^".json" in
  Yojson.Basic.to_file (path^"Stats/"^filename) stats_json;
  `String filename

let get_health s =
  s.health

let get_base_stats id =
  {id = id; health = 100.; strength = 5.; speed = 5.; dexterity = 5.; magic = 0.}

let get_stats_list (st: t): string list =
  ["health: " ^ string_of_float st.health; "strength: " ^ string_of_float st.strength;
  "speed: " ^ string_of_float st.speed; "dexterity: " ^ string_of_float st.dexterity;
  "magic: " ^ string_of_float st.magic]

let print_stats id s =
  let stats_list = get_stats_list s in
  let f str = printf "\t%s\n" str in
  printf "\n%s Stats:" id;
  List.iter f stats_list

let to_str st : string =
  String.concat "\n" (get_stats_list st)

let print_battle_stats (ustats: t) (ostats: t) =
  let user_stats = get_stats_list ustats in
  let opp_stats = get_stats_list ostats in
  let f str1 str2 = printf "\t%s\t\t\t\t\t%s\n" str1 str2 in
  printf "\nUser Stats:\t\t\t\t\tOpponent Stats:\n";
  List.iter2 f user_stats opp_stats

let combine (st1: t) (st2: t) : t =
  {
  id = st2.id;
  health = if (st1.health +. st2.health) < 0. then 0. else st1.health +. st2.health;
  strength = st1.strength +. st2.strength;
  speed = st1.speed +. st2.speed;
  dexterity = st1.dexterity +. st2.dexterity;
  magic = st1.magic +. st2.magic
  }

let apply resistance_effect self_effect opp_effect self_stats opp_stats : (t * t) =
  let new_self_stats = {
  self_stats with
  health = self_stats.health +. self_effect.health;
  strength = self_stats.strength /. (1.-.(self_effect.strength/.100.));
  speed = self_stats.speed /. (1.-.(self_effect.speed/.100.));
  dexterity = self_stats.dexterity /. (1.-.(self_effect.dexterity/.100.));
  magic = self_stats.magic /. (1.-.(self_effect.magic/.100.));
  } in

  (* print_stats "resistance_effect" resistance_effect; *)
  (* print_stats "self_effect" self_effect; *)
  (* print_stats "opponent_effect" opp_effect; *)

  let new_opp_stats = {
  opp_stats with
  strength = opp_stats.strength /. (1.-.(opp_effect.strength/.100.));
  speed = opp_stats.speed /. (1.-.(opp_effect.speed/.100.));
  dexterity = opp_stats.dexterity /. (1.-.(opp_effect.dexterity/.100.));
  magic = opp_stats.magic /. (1.-.(opp_effect.magic/.100.));
  } in

  let damage_multiplier = (
    (1.-.(new_opp_stats.strength /. 100.))
      /. (1.-.(new_self_stats.strength /. 100.))
      /. (1.-.(resistance_effect.strength /. 100.)) +.
    (1.-.(new_opp_stats.speed /. 100.))
      /. (1.-.(new_self_stats.speed /. 100.))
      /. (1.-.(resistance_effect.speed /. 100.)) +.
    (1.-.(new_opp_stats.dexterity /. 100.))
      /. (1.-.(new_self_stats.dexterity /. 100.))
      /. (1.-.(resistance_effect.dexterity /. 100.)) +.
    (1.-.(new_opp_stats.magic /. 100.))
      /. (1.-.(new_self_stats.magic /. 100.))
      /. (1.-.(resistance_effect.magic /. 100.))) 
  /. ((1. /. (1. -. (resistance_effect.strength /. 100.))) +.
      (1. /. (1. -. (resistance_effect.speed /. 100.))) +.
      (1. /. (1. -. (resistance_effect.dexterity /. 100.))) +.
      (1. /. (1. -. (resistance_effect.magic /. 100.)))) in

  let newer_opp_stats = {
  new_opp_stats with
  health = new_opp_stats.health +. (opp_effect.health *. damage_multiplier);
  } in
  (* printf "opp_effect.health *. damage_multiplier = %f * %f = %f\n" opp_effect.health damage_multiplier (opp_effect.health *. damage_multiplier); *)
  (new_self_stats, newer_opp_stats)


let difference (st1: t) (st2: t) : float =
  (st1.health -. st2.health) +.
  (st1.strength -. st2.strength) +.
  (st1.speed -. st2.speed) +.
  (st1.dexterity -. st2.dexterity) +.
  (st1.magic -. st2.magic)