open Yojson.Basic.Util
open Printf

type t = {
  id: string;
  money: int;
  equipped: Item.t list;
  inventory: Item.t list;
}


let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Players/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let money = json |> member "money" |> to_int in
  let inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  {
  id;
  money;
  equipped;
  inventory;
  }

let to_file path player =
  failwith "TODO"


let stats (p: t) =
  failwith "this didn't compile"
(*   let blank_stats =
  {id = "Total Stats"; health = 0; strength = 0; speed = 0; dexterity = 0; magic = 0} in
  List.fold_left (let f acc i = Stats.combine i.Item.base_effect acc) blank_stats p.equipped
 *)

let rec print_item_list p = 
  match p with 
  | [] -> ()
  | h::t -> printf "Item: %s\n" (Item.get_id h); 
            printf "  Description: %s\n" (Item.get_description h); 
            printf "  Value: %d\n" (Item.get_value h);
            print_item_list t

let print_score p =
  printf "money: %d\n" p.money;
  printf "Inventory:\n";
  print_item_list p.inventory;
  printf "Equipped:\n";
  print_item_list p.equipped;


