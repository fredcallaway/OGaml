open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  fighter: Fighter.t;
  money: int;
  stats: Stats.t;
  inventory: Item.t list;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Players/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let fighter = json |> member "fighter" |> to_string |> Fighter.from_file path in
  let money = json |> member "money" |> to_int in
  let stats = json |> member "stats" |> to_string |> Stats.from_file path in
  let inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  {
  id;
  fighter;
  money;
  inventory
  }

let to_file path player =
  failwith "TODO"

let get_fighter (p: t) = p.fighter

let stats (p: t) = 
  List.fold_left (let f acc i = (Stats.combine acc i.Item.))

let print_score p =
  printf "money: %d\n" p.money;


