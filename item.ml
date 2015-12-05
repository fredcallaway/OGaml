open Yojson.Basic.Util
open Printf

(* Fred: I simplified things a lot. Feel free to add stuff back,
 * as long as you keep the get_effects function. I think the
 * distinction between equippable and usable is unnecessary however.
 * I include consumable items in equip because this will make
 * it easier to list all the items the player can use.
 * The Consumable slot can be filled with many items. *)
type slot =
  | Consumable
  | Head
  | Body
  | Legs
  | Feet
  | Hands
  | Primary
  | Secondary
  | Special

exception InvalidSlot of string
exception InvalidItem of string

let str_to_slot str =
  match str with
  | "Consumable" -> Consumable
  | "Head" -> Head
  | "Body" -> Body
  | "Legs" -> Legs
  | "Feet" -> Feet
  | "Hands" -> Hands
  | "Primary" -> Primary
  | "Secondary" -> Secondary
  | "Special" -> Special
  | _ -> raise (InvalidSlot str)

let slot_to_str slot =
  match slot with
  | Consumable -> "Consumable"
  | Head -> "Head"
  | Body -> "Body"
  | Legs -> "Legs"
  | Feet -> "Feet"
  | Hands -> "Hands"
  | Primary -> "Primary"
  | Secondary -> "Secondary"
  | Special -> "Special"

type t = {
  id: string;
  description: string;
  base_effect: Stats.t;
  resistance_effect: Stats.t;
  self_effect: Stats.t;
  opponent_effect: Stats.t;
  value: int;
  slot: slot;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Items/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let description = json |> member "description" |> to_string in
  let base_effect = json |> member "base_effect" |> Stats.from_json (id^" base_effect") in
  let resistance_effect = json |> member "resistance_effect" |> Stats.from_json (id^" resistance_effect") in
  let self_effect = json |> member "self_effect" |> Stats.from_json (id^" self_effect") in
  let opponent_effect = json |> member "opponent_effect" |> Stats.from_json (id^" opponent_effect") in
  let value = json |> member "value" |> to_int in
  let slot = json |> member "slot" |> to_string |> str_to_slot in
  {
  id;
  description;
  base_effect;
  resistance_effect;
  self_effect;
  opponent_effect;
  value;
  slot
  }

let to_file path item =
  let description_json = `String (item.description) in
  let base_effect_json = item.base_effect |> Stats.to_json in
  let resistance_effect_json = item.resistance_effect |> Stats.to_json in
  let self_effect_json = item.self_effect |> Stats.to_json in
  let opponent_effect_json = item.opponent_effect |> Stats.to_json in
  let value_json = `Int (item.value) in
  let slot_json = `String (item.slot |> slot_to_str) in
  let item_json = `Assoc [
    ("description", description_json);
    ("base_effect", base_effect_json);
    ("resistance_effect", resistance_effect_json);
    ("self_effect", self_effect_json);
    ("opponent_effect", opponent_effect_json);
    ("value", value_json);
    ("slot", slot_json)
  ] in
  let filename = item.id^".json" in
  Yojson.Basic.to_file (path^"Items/"^filename) item_json;
  `String filename

(* a set of equipped armor and weapons *)
(* type equip = t list *)

let get_id (i: t) = i.id

let get_description (i: t) = i.description

let get_base_effect (i: t) = i.base_effect

let get_self_effect (i: t) = i.self_effect

let get_opponent_effect (i: t) = i.opponent_effect

let get_resistance_effect (i: t) = i.resistance_effect

let get_value (i: t) = i.value

let is_consumable (i: t) =
  match i.slot with
  | Consumable -> true
  | _ -> false

let get_slot_item (item: t) (equipped: t list): t option =
  if is_consumable item then
    let consumables = List.filter (is_consumable) equipped in
    if (List.length consumables) = 3 then Some (List.hd consumables)
    else None
  else None


let rec get_item (id: string) (selection: t list): t option =
  match selection with
  | h::t -> if (String.lowercase id) = (String.lowercase h.id) then Some h
            else get_item id t
  | [] -> None

let rec remove (lst: t list) (i: t) =
  match lst with
  |[] -> raise (InvalidItem i.id)
  |hd::tl -> if hd.id = i.id then tl else hd::(remove tl i)


let rec str_to_item is str =
  match is with
  | [] -> raise (InvalidItem str)
  | hd::tl -> if String.lowercase hd.id = str then hd else str_to_item tl str

let print_item i =
  printf "%s Stats" i.id;
  Stats.print_stats "base_effect" i.base_effect;
  Stats.print_stats "resistance_effect" i.resistance_effect;
  Stats.print_stats "self_effect" i.self_effect;
  Stats.print_stats "opponent_effect" i.opponent_effect

let print_double_item_list (ulst: t list) (olst: t list) =
  let nth_catch l n = try List.nth l n with |Failure str -> "___________" in
  let slot_list = [(Consumable, 3); (Head, 1); (Body, 1); (Legs, 1); (Feet, 1); (Hands, 1); (Primary, 1); (Secondary, 1); (Special, 1)] in
  let get_all_slot sl is = List.filter (fun x -> x.slot = sl) is in
  let g it = it.id in
  let f (s, m) = (s, m, List.map g (get_all_slot s ulst), List.map g (get_all_slot s olst)) in
  let big_list = List.map f slot_list in
  let create_slot_block sl max l1 l2 =
    printf "\n%s:\n" (slot_to_str sl);
    for i = 0 to max-1 do
      printf "\t%s\t\t\t\t\t%s\n" (nth_catch l1 i) (nth_catch l2 i);
    done
  in
  printf "User Inventory:\t\t\t\t\tOpponent Inventory:\n";
  List.iter (fun (s, m, l1, l2) -> create_slot_block s m l1 l2) big_list