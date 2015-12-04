open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  stats: Stats.t;
  money: int;
  equipped: Item.t list;
  inventory: Item.t list;
}


let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Players/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let stats = json |> member "stats" |> Stats.from_json (id^" stats") in
  let money = json |> member "money" |> to_int in
  let inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  {
  id;
  stats;
  money;
  equipped;
  inventory;
  }

let to_file path player =
  let stats_json = player.stats |> Stats.to_json in
  let money_json = `Int (player.money) in
  let inventory_json = `List (player.inventory |> List.map (Item.to_file path)) in
  let equipped_json = `List (player.equipped |> List.map (Item.to_file path)) in
  let player_json = `Assoc [
    ("stats", stats_json);
    ("money", money_json);
    ("inventory", inventory_json);
    ("equipped", equipped_json)
  ] in
  let filename = player.id^".json" in
  Yojson.Basic.to_file (path^"Players/"^filename) player_json;
  `String filename


let stats (p: t) =
  failwith "this didn't compile"
(*   let blank_stats =
  {id = "Total Stats"; health = 0; strength = 0; speed = 0; dexterity = 0; magic = 0} in
  List.fold_left (let f acc i = Stats.combine i.Item.base_effect acc) blank_stats p.equipped
 *)

let rec print_item_list p =
  match p with
  | [] -> ()
  | h::t -> printf "%s:\n" (Item.get_id h);
            printf "  Description: %s\n" (Item.get_description h);
            printf "  Value: %d\n" (Item.get_value h);
            print_item_list t

let print_bag p =
  printf "\nMoney: %d\n" p.money;
  printf "\nInventory:\n";
  print_item_list p.inventory;
  printf "\nEquipped:\n";
  print_item_list p.equipped;


