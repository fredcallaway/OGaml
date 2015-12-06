open Player
open Assertions
open Printf



let testdir = "InitGames/game1/"

let test_player filename =
  (* try *)
    let i = Player.from_file testdir filename in
    ignore i
  (* with *)
  (* | Yojson.Json_error(str) -> *)
    (* printf "############### %s ################\n" filename *)

let test_all_players () =
  let players = Sys.readdir (testdir^"Players/") in
  Array.iter test_player players



let () =
  test_all_players ();
  ()