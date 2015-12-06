open World
open Assertions
open Printf

let testdir = "InitGames/game1/"
let w1 = World.from_file testdir "OGaml.json"
let p1 = Player.from_file testdir "player0.json"

let test_map () =
  print_map w1

let test_win () =
  print_win w1

let () =
  test_map();
  (* test_win(); *)
  ()