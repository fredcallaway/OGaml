open Battle
open Assertions
open Printf

let testdir = "SavedGames/game2/"

let bow = Item.from_file testdir "Bow.json"
let sword = Item.from_file testdir "Sword.json"
let nail = Item.from_file testdir "Rusty Nail.json"
let health_potion = Item.from_file testdir "Health Potion.json"

let f0 = Fighter.from_file testdir "fighter0.json"
let f1 = Fighter.from_file testdir "fighter1.json"
let f2 = Fighter.from_file testdir "fighter2.json"
let f3 = Fighter.from_file testdir "fighter3.json"


let test_apply_effects () =
  Stats.print_battle_stats (Fighter.get_stats f2) (Fighter.get_stats f3);
  let f2', f3' = apply_effects bow (f2, f3) in
  (* f2' === f2; *)
  Stats.print_battle_stats (Fighter.get_stats f2') (Fighter.get_stats f3');
  (* printf "%f" *)
  (* Fighter.health f3' === Fighter.health f3 - 10.; *)

  (* let f2', f3' = apply_effects health_potion (f2, f3) in *)
  (* f3' === f3; *)
  (* Fighter.health f2' === Fighter.health f2 + 20.; *)
  ()


let test_use_item () =
  let switch (a,b) = (b,a) in
  (* let f2_stats = Fighter.get_stats f2 in *)
  use_item true (f1, f2) sword === apply_effects sword (f1, f2);
  use_item false (f1, f2) sword === switch (apply_effects sword (f2, f1));

  (* test item removal *)
  let f1', f2' = use_item true (f1, f2) health_potion in
  Fighter.get_equipped f1' === [sword];
  f2' === f2;

  let f1', f2' = use_item false (f1, f2) health_potion in
  f1' === f1;
  Fighter.get_equipped f2' === [health_potion; bow];
  ()


let test_ai_value_heuristic () =
  ai_value_heuristic true (f1, f2) === -50;
  ai_value_heuristic false (f2, f1) === -50;
  ()


let test_ai_value () =
  ai_value true 0 (f0, f0) === 0;
  ai_value true 1 (f0, f0) === 1;
  ai_value false 2 (f0, f0) === 0;
  ai_value false 3 (f0, f0) === 1;
  ()


let test_get_ai_action () =
  get_ai_action true 0 (f2, f3) === health_potion;
  get_ai_action true 3 (f2, f3) === health_potion;

  get_ai_action false 0 (f2, f3) === sword;
  get_ai_action false 3 (f2, f3) === sword;
  ()


let () =
  test_apply_effects ()
  (* test_use_item () *)
  (* test_ai_value_heuristic () *)
  (* test_ai_value () *)
  (* test_get_ai_action (); *)


