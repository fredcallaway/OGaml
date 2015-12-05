open Yojson.Basic.Util
open Printf

type t = {
  id : string;
  world : World.t;
  player : Player.t;
}
exception InvalidCommand of string
exception InvalidGame of string
exception InvalidName of string

let from_file path filename =
  let json = Yojson.Basic.from_file (path^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let world = json |> member "world" |> to_string |> World.from_file path in
  let player = json |> member "player" |> to_string |> Player.from_file path in
  {
  id;
  world;
  player
  }

(* let pretty_to_file filename json =
  Yojson.pretty_to_string
  stream_from_string
  stream_to_file filename stream *)

let to_file game =
  let path = "SavedGames/" ^ game.id ^ "/" in
  let world_json = game.world |> World.to_file path in
  let player_json = game.player |> Player.to_file path in
  let game_json = `Assoc [
    ("world", world_json);
    ("player", player_json)
  ] in
  Yojson.Basic.to_file (path^game.id^".json") game_json;
  printf "Game saved successfully.\n"

let mkdir path dir =
  let permissions = 0o777 in
  Unix.mkdir (path^dir) permissions

let new_game init_name new_name =
  if String.length new_name = 0
  then raise (InvalidName new_name)
  else ();

  let init_path = "InitGames/" ^ init_name ^ "/" in
  let init_game = from_file init_path (init_name^".json") in
  printf "Initial game %s loaded.\n" init_game.id;
  let new_game = {init_game with id=new_name} in

  (* create new game dirs *)
  mkdir "SavedGames/" new_name;
  let game_path = "SavedGames/"^new_name^"/" in
  mkdir game_path "Battles";
  mkdir game_path "Fighters";
  mkdir game_path "Items";
  mkdir game_path "Players";
  mkdir game_path "Shops";
  mkdir game_path "Worlds";
  mkdir game_path "Zones";

  to_file new_game;
  printf "New game created as %s.\n" new_game.id

type command = New | Save | List | Load | Enter | Bag | Quit | Help
let cmds = [  "New";"Save";"List";"Load";"Enter";"Bag";"Quit"]

let str_to_command str : command =
  match str with
  | "new" -> New
  | "save" -> Save
  | "list" -> List
  | "load" -> Load
  | "enter" -> Enter
  | "bag" -> Bag
  | "quit" -> Quit
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "new" -> "Create a new game and load it."
  | "save" -> "Save the loaded game."
  | "list" -> "List all avilible games to load."
  | "load" -> "Load a game from a json file."
  | "enter" -> "Enter the loaded game."
  | "bag" -> "Check your player's bag in the loaded game."
  | "quit" -> "Quit the entire program."
  | _ -> raise (InvalidCommand str)

let print_help (arg: string) =
  let print_helper cmd =
    printf "%s:\n" cmd;
    printf "%s\n\n" (str_to_help cmd);
  in
  if arg = ""
  then List.iter print_helper cmds
  else print_helper arg

let print_commands () =
  printf "Availible commands: (type 'Help [cmd]' to get more info)\n";
  List.iter (printf "%s\n") (cmds)

let print_welcome () =
  printf "\n\nWelcome to OGaml!\n\n"

let print_return (game: t) =
  printf "You've returned to the main menu.\n"

let print_list () =
  let dir = "SavedGames/" in
  let games = Sys.readdir dir in
  printf "Availible games: (type 'Load [game]' to load game)\n";
  Array.iter print_endline games


let rec game_repl (gameop: t option) : t option =
  try
    let cmd, arg = Io.get_input () in
    match (str_to_command cmd),gameop with

    | Help, _ ->
      print_help arg;
      game_repl gameop

    | Bag, Some game ->
      Player.print_bag game.player;
      game_repl gameop

    | Enter, Some game ->
      printf "Entering %s\n\n" game.id;
      let updated_state = World.enter_world game.world game.player in
      let updated_world = (fst updated_state) in
      let updated_player = (snd updated_state) in
      let updated_game = {game with world = updated_world; player = updated_player} in
      print_return updated_game;
      game_repl (Some updated_game)

    | Save, Some game ->
      to_file game;
      game_repl gameop

    | Load, _ ->
      let filename = arg in
      let path = "SavedGames/" ^ filename ^ "/" in
      let load_game = from_file path (filename^".json") in
      printf "Game loaded successfully.\n";
      printf "Type 'Enter' to continue game.\n";
      game_repl (Some load_game)

    | List, _ ->
      print_list();
      game_repl gameop

    | New, _ ->
      let new_name = arg in
      let init_name = "game1" in
      new_game init_name new_name;
      game_repl gameop

    | Quit, _ ->
      printf "Quitting\n";
      gameop

    | _, None -> raise (InvalidGame "None")

  with
    | InvalidCommand str ->
      printf "\nInvalid command: %s\n" str;
      game_repl gameop

    | InvalidGame str ->
      printf "\nInvalid game: %s\n" str;
      printf "Please load a valid game file.\n";
      game_repl gameop

    | InvalidName str ->
      printf "\nInvalid name: %s \nPlease enter a valid new name.\n" str;
      game_repl gameop

    | Unix.Unix_error (err, cmd, arg) ->
      printf "Cannot create %s. \nA game with the same name already exists.\n" arg;
      game_repl gameop

    | Sys_error str ->
      printf "\n%s\n" str;
      printf "Please load a valid game file.\n";
      game_repl gameop

    | Failure str ->
      printf "\nFailure: %s\n" str;
      game_repl gameop

(* start the game with no game state *)
(* postcondition: the updated game on exit *)
let enter_game () =
  print_welcome();
  print_commands();
  try mkdir "" "SavedGames"; with | _ -> ();
  ignore (game_repl None);
in

enter_game()