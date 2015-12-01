open Battle
open Assertions

let testdir = "SavedGames/TestGame/"

let test_get_ai_action () =
  let f1 = Fighter.from_file testdir "fighter2.json" in
  let f2 = Fighter.from_file testdir "fighter3.json" in
  let item = get_ai_action true (f1, f2) in
  Item.get_id item === "Health Potion";
  4 === 3;
  ()

let test_apply_effects () = 
  let bow = Item.from_file testdir "Bow" in
  let f1 = Fighter.from_file testdir "fighter2.json" in
  let f2 = Fighter.from_file testdir "fighter3.json" in
  let f1', f2' = apply_effects bow f1 f2 in

  f1 === f1';
  Fighter.health f2' === Fighter.health f2 - 10;
  ()


let () =
  test_get_ai_action ()