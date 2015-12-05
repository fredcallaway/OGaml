open Yojson.Basic.Util

type t = {
  id: string;
  stats: Stats.t;
  equipped: Item.t list;
}

let apply_base_effects (f: t) : t =
  let base_stats = Stats.get_base_stats (f.id^" stats") in
  let g acc i = Stats.combine i.Item.base_effect acc in
  let new_stats = List.fold_left g base_stats f.equipped in
  {f with stats=new_stats}

let make (player: Player.t) : t =
  {
  id=player.Player.id;
  stats=Stats.get_base_stats (player.Player.id^" stats");
  equipped=player.Player.equipped
  }

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Fighters/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let stats = json |> member "stats" |> Stats.from_json (id^" stats") in
  let equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  {
  id;
  stats;
  equipped;
  }

let to_file path fighter =
  let stats_json = fighter.stats |> Stats.to_json in
  let equipped_json = `List (fighter.equipped |> List.map (Item.to_file path)) in
  let fighter_json = `Assoc [
    ("stats", stats_json);
    ("equipped", equipped_json)
  ] in
  let filename = fighter.id^".json" in
  Yojson.Basic.to_file (path^"Fighters/"^filename) fighter_json;
  `String filename

let get_equipped (f: t) = f.equipped

let set_equipped (f: t) (eq: Item.t list) =
  {f with equipped = eq}

let get_stats (f: t) = f.stats

let set_stats (f: t) (newstats: Stats.t)= {f with stats = newstats}

let alive f : bool = Stats.get_health f.stats > 0.

(* let apply_effect effect (f : t) =
  {f with stats = (effect f.stats)} *)


let health (f:t): float =
  f.stats.Stats.health
let strength (f:t): float =
  f.stats.Stats.strength
let speed (f:t): float =
  f.stats.Stats.speed
let dexterity (f:t): float =
  f.stats.Stats.dexterity
let magic (f:t): float =
  f.stats.Stats.magic