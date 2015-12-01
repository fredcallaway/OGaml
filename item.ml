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
  self_effect: Stats.t;
  opponent_effect: Stats.t;
  (* base_effect: Stats.t; *)
  value: int;
  slot: slot;
  quantity: int;
  (* TODO: eliminate quantity *)
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Items/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let description = json |> member "description" |> to_string in
  let self_effect = json |> member "self_effect" |> Stats.from_json (id^" self_effect") in
  let opponent_effect = json |> member "opponent_effect" |> Stats.from_json (id^" opponent_effect") in
  let value = json |> member "value" |> to_int in
  let slot = json |> member "slot" |> to_string |> str_to_slot in
  let quantity = json |> member "quantity" |> to_int in
  {
  id;
  description;
  self_effect;
  opponent_effect;
  value;
  slot;
  quantity
  }

let to_file path item =
  failwith "TODO"

(* a set of equipped armor and weapons *)
(* type equip = t list *)

let get_id (i: t) = i.id

let get_description (i: t) = i.description

let get_self_effect (i: t) = i.self_effect

let get_opponent_effect (i: t) = i.opponent_effect

let get_slot_item x y = failwith "unimplemented"


let rec get_item (id: string) (selection: t list): t option =
  match selection with 
  | h::t -> if (String.lowercase id) = (String.lowercase h.id) then Some h
            else get_item id t
  | [] -> None

let rec remove (lst: t list) (i: t) =
  match lst with
  |[] -> []
  |hd::tl -> if hd.id = i.id then match hd.quantity with
                                  |1 -> tl
                                  |_ -> {hd with quantity = hd.quantity - 1}::tl
              else hd:: remove tl i

let is_consumable (i: t) =
  match i.slot with
  | Consumable -> true
  | _ -> false

let rec str_to_item is str =
  match is with
  | [] -> raise (InvalidItem str)
  | hd::tl -> if String.lowercase hd.id = str then hd else str_to_item tl str

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

let get_effects item : Stats.effect * Stats.effect =
  failwith "TODO"
  (* Many ways this function could be implemented. The simplest would be
   * to match on every possible item id. A better option would be to
   * get the information from the json somehow. *)
  (* match item.id with *)
  (* | "health potion" -> health_effect 10, null_effect *)
  (* | "frighten" -> null_effect, strength_effect (-5) *)
  (* | _ -> null_effect, null_effect *)
