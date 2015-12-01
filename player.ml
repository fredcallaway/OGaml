open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  fighter: Fighter.t;
  money: int;
  stats: Stats.t;
  inventory: Item.t list;
  expereience: int;
  level: int;
}


let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Players/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let fighter = json |> member "fighter" |> to_string |> Fighter.from_file path in
  let money = json |> member "money" |> to_int in
  let stats = json |> member "stats" |> to_string |> Stats.from_file path in
  let inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let expereience = json |> member "expereience" |> to_int in
  let level = json |> member "level" |> to_int in
  {
  id;
  fighter;
  money;
  stats;
  inventory;
  expereience;
  level
  }

let to_file path player =
  failwith "TODO"

let get_fighter (p: t) = p.fighter

let print_score p =
  printf "money: %d\n" p.money;
  printf "expereience: %d\n" p.expereience;
  printf "level: %d\n" p.level

(* must keep track of level up schedule.. maybe in state? *)