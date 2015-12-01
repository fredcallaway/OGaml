open Battle


let test_get_ai_action () =
  let p1 = Fighter.from_file "SavedGames/TestGame/" "fighter2.json" in
  let p2 = Fighter.from_file "SavedGames/TestGame/" "fighter3.json" in
  let item = get_ai_action true (p1, p2) in
  print_endline @@ Item.get_description item










let () =
  test_get_ai_action ()