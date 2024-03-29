open Battle
open Assertions
open Printf

open Testing_utils (* populates name space with fighters and items *)

let test_apply_effects () =
  Stats.print_battle_stats (Fighter.get_stats f2) (Fighter.get_stats f3);
  Item.print_item bow;
  let f2', f3' = apply_effects bow (f2, f3) in
  f2' === f2;

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
  use_item false (f1, f2) nail === switch (apply_effects nail (f2, f1));
  use_item false (f1, f2) sword === switch (apply_effects sword (f2, f1));

  (* test item removal *)
  let id_list equip = List.map (fun a -> a.Item.id) equip in

  let f1', f2' = use_item true (f1, f2) health_potion in
  id_list (Fighter.get_equipped f1') === id_list [sword];
  f2' === f2;

  let f1', f2' = use_item false (f1, f2) health_potion in
  f1' === f1;
  id_list (Fighter.get_equipped f2') === id_list [health_potion; bow];
  ()


let test_ai_value_heuristic () =
  ai_value_heuristic true (f1, f2) === -50.0;
  ai_value_heuristic false (f1, f2) === 50.0;
  ai_value_heuristic true (f2, f1) === 50.0;
  ai_value_heuristic false (f2, f1) === -50.0;
  ()


let test_ai_value () =
  ai_value 0 true (f0, f0) === 0.0;
  ai_value 1 true (f0, f0) === 1.0;
  ai_value 2 false (f0, f0) === 0.0;
  ai_value 3 false (f0, f0) === 1.0;


(*   ai_value true 0 (f1, f1) === 0.0;
  ai_value true 1 (f1, f1) === 20.0;
  ai_value false 2 (f1, f1) === 0.0;
  ai_value false 3 (f1, f1) === 5.0; *)
  ()


let test_get_ai_action () =
  (* get_ai_action true 0 (f2, f3) === health_potion; *)
  (* get_ai_action false 1 (f2, f3) === health_potion; *)

  get_ai_action true 2 (f5, f3) === sword;
  ()

let test_load_battle () =
  let testdir = "InitGames/game1/" in
  let b0 = Battle.from_file testdir "battle1.json" in
  ignore b0;
  ()

let testdir = "InitGames/game1/"

let test_battle filename =
  try
    let b = Battle.from_file testdir filename in
    ignore b
  with
  | Yojson.Json_error(str) ->
    printf "############### %s ################\n" filename

let test_all_battles () =
  let battles = Sys.readdir (testdir^"Battles/") in
  Array.iter test_battle battles

let () =
  test_apply_effects ();
  test_use_item ();
  test_ai_value_heuristic ();
  test_ai_value ();
  test_get_ai_action ();


