open Yojson.Basic.Util

type t = {
  id: string;
  stats: Stats.t;
  inventory: Item.t list;
  equipped: Item.t list;
  hp: int;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Fighters/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let stats = json |> member "stats" |> to_string |> Stats.from_file path in
  let inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let hp = json |> member "hp" |> to_int in
  (* let effects = json |> member "effects" |> to_list |> List.map effect_obj path in *)
  {
  id;
  stats;
  inventory;
  equipped;
  hp
  (* effects *)
  }

let to_file path game =
  failwith "TODO"

let alive f : bool = Stats.get_health f.stats > 0

let remove_item f item : t = failwith "unimplemented"

let apply_effect effect (f : t) =
  {f with stats = (effect f.stats)}