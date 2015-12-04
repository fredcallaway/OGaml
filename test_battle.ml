open Battle
open Assertions

let testdir = "SavedGames/game1/"
let bow = Item.from_file testdir "Bow.json"
let health_potion = Item.from_file testdir "Health Potion.json"
let f2 = Fighter.from_file testdir "fighter2.json"
let f3 = Fighter.from_file testdir "fighter3.json"

let test_apply_effects () = 
  let f2', f3' = apply_effects bow f2 f3 in
  f2 === f2';
  Fighter.health f3' === Fighter.health f3 - 10;
  ()

let test_get_ai_action () =
  let item = get_ai_action true 0 (f2, f3) in
  Item.get_id item === "Health Potion";
  let item = get_ai_action false 0 (f2, f3) in
  Item.get_id item === "Sword";
  ()


let () =
  test_get_ai_action ();
  test_apply_effects ()


