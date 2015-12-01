open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  money: int;
  equipped: Item.t list;
  inventory: Item.t list;
  expereience: int;
  level: int;
}

let stats (p: t) : Stats.t = failwith "unimplemented"

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Players/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let money = json |> member "money" |> to_int in
  let inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let expereience = json |> member "expereience" |> to_int in
  let level = json |> member "level" |> to_int in
  let equipped = [] in
  {
  id;
  money;
  equipped;
  inventory;
  expereience;
  level
  }

let to_file path player =
  failwith "TODO"

let print_score p =
  printf "money: %d\n" p.money;
  printf "expereience: %d\n" p.expereience;
  printf "level: %d\n" p.level

(* must keep track of level up schedule.. maybe in state? *)