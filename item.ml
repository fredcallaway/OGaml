open Stats


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

type t = {id: string;
          description: string;
          self_effect: stats;
          opponent_effect: stats;
          value: int;
          slot: slot}

(* a set of equipped armor and weapons *)
type equip = t list

let get_effects item : effect * effect =
  (* Many ways this function could be implemented. The simplest would be
   * to match on every possible item id. A better option would be to
   * get the information from the json somehow. *)
  match item.id with
  | "health potion" -> health_effect 10, null_effect
  | "frighten" -> null_effect, strength_effect (-5)
  | _ -> null_effect, null_effect
