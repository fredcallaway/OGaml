open Zone
open Assertions
open Printf

let testdir = "SavedGames/game1/"
let z1 = Zone.from_file testdir "zone1.json"
let p1 = Player.from_file testdir "player0.json"

let test_map () =
  print_map z1

let test_win () =
  print_win z1

let () =
  test_map ();