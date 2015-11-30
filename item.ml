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

type t = {
  id: string;
  description: string;
  self_effect: Stats.t;
  opponent_effect: Stats.t;
  value: int;
  slot: slot;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Items/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let description = json |> member "description" |> to_string in
  let self_effect = json |> member "self_effect" |> Stats.from_json (id^" self_effect") in
  let opponent_effect = json |> member "opponent_effect" |> Stats.from_json (id^" opponent_effect") in
  let value = json |> member "value" |> to_int in
  let slot = json |> member "slot" |> to_string |> str_to_slot in
  {
  id;
  description;
  self_effect;
  opponent_effect;
  value;
  slot
  }

let to_file path item =
  failwith "TODO"

(* a set of equipped armor and weapons *)
(* type equip = t list *)

let get_description (i: t) = i.description

let is_consumable (i: t) =
  match i.slot with
  | Consumable -> true
  | _ -> false

let rec str_to_item is str =
  match is with
  | [] -> raise (InvalidItem str)
  | hd::tl -> if String.lowercase hd.id = str then hd else str_to_item tl str

let print_double_item_list (ulst: t list) (olst: t list) =
  let f it = it.id in
  let user_equip = List.map f ulst in
  let opp_equip = List.map f olst in
  let g str1 str2 = printf "%s\t\t\t\t\t%s\n" str1 str2 in
  List.iter2 g user_equip opp_equip

let get_effects item : Stats.effect * Stats.effect =
  failwith "TODO"
  (* Many ways this function could be implemented. The simplest would be
   * to match on every possible item id. A better option would be to
   * get the information from the json somehow. *)
  (* match item.id with *)
  (* | "health potion" -> health_effect 10, null_effect *)
  (* | "frighten" -> null_effect, strength_effect (-5) *)
  (* | _ -> null_effect, null_effect *)
