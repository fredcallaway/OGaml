open Yojson.Basic.Util

type t = {
  id: string;
  stats: Stats.t;
  equipped: Item.t list;
}

let make (player: Player.t) : t =
  {id="foo";
   stats=Player.stats player;
   equipped=player.Player.equipped}

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
  failwith "TODO"

let get_equipped (f: t) = f.equipped

let set_equipped (f: t) (eq: Item.t list) =
  {f with equipped = eq}

let get_stats (f: t) = f.stats

let set_stats (f: t) (newstats: Stats.t)= {f with stats = newstats}

let alive f : bool = Stats.get_health f.stats > 0

let apply_effect effect (f : t) =
  {f with stats = (effect f.stats)}


let health (f:t): int =
  f.stats.Stats.health
let strength (f:t): int =
  f.stats.Stats.strength
let speed (f:t): int =
  f.stats.Stats.speed
let dexterity (f:t): int =
  f.stats.Stats.dexterity
let magic (f:t): int =
  f.stats.Stats.magic