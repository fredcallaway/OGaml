open Yojson.Basic.Util

type t = {
  id: string;
  stats: Stats.t;
  equipped: Item.t list;
  hp: int;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Fighters/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let stats = json |> member "stats" |> to_string |> Stats.from_file path in
  let equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let hp = json |> member "hp" |> to_int in
  (* let effects = json |> member "effects" |> to_list |> List.map effect_obj path in *)
  {
  id;
  stats;
  equipped;
  hp
  (* effects *)
  }

let to_file path fighter =
  failwith "TODO"

let get_equipped (f: t) = f.equipped

let get_stats (f: t) = f.stats

let alive f : bool = Stats.get_health f.stats > 0

let remove_item f item : t = failwith "unimplemented"

let apply_effect effect (f : t) =
  {f with stats = (effect f.stats)}