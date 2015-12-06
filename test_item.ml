open Item
open Assertions
open Printf



let testdir = "InitGames/game1/"

let test_item filename =
  try
    let i = Item.from_file testdir filename in
    ignore i
  with
  | Yojson.Json_error(str) ->
    printf "############### %s ################\n" filename

let test_all_items () =
  let items = Sys.readdir (testdir^"Items/") in
  Array.iter test_item items



let () =
  test_all_items ();
  ()