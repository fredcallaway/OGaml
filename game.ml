open Printf

type t = {
  id : string;
  world : World.t;
  player : Player.t;
}

type command = Enter | New | Save | Load | List | Quit | Score | Help
let cmds = [  "Enter";"New";"Save";"Load";"List";"Quit";"Score"]
exception InvalidCommand of string
exception InvalidGame of string

let str_to_command str : command =
  match str with
  | "enter" -> Enter
  | "new" -> New
  | "save" -> Save
  | "load" -> Load
  | "list" -> List
  | "quit" -> Quit
  | "score" -> Score
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "enter" -> "Enter the loaded game."
  | "new" -> "Create a new game and load it."
  | "save" -> "Save the loaded game."
  | "load" -> "Load a game from a json file."
  | "list" -> "List all avilible games to load."
  | "quit" -> "Quit the entire program."
  | "score" -> "Display the score from the loaded game."
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

let print_welcome (game: t) =
  printf "Welcome to OGaml!\n"

let print_return (game: t) =
  printf "You've returned to the main menu.\n"

let print_score (player: Player.t) =
  failwith "TODO"


let rec game_repl (gameop: t option) : t option =
  try
    (* prompt user for command *)
    print_endline "\nWhats Next?";
    (* get input line *)
    let line = String.lowercase (input_line stdin) in
    print_endline "\n";

    if String.length line = 0 then raise (InvalidCommand line) else ();

    (* split the input into command and args *)
    let split = Str.bounded_split (Str.regexp " ") line 2 in
    let has_arg = List.length split > 1 in

    let cmd = str_to_command (List.nth split 0) in
    let arg = if has_arg then List.nth split 1 else "" in

    (* Command Switch *)
    match cmd,gameop with

    | Help, _ ->
      print_help arg;
      game_repl gameop

    | Score, Some game ->
      print_score game.player;
      game_repl gameop

    | Enter, Some game ->
      printf "Entering %s\n" game.id;
      let new_state = World.enter_world game.world game.player in
      let new_world = (fst new_state) in
      let new_player = (snd new_state) in
      let new_game = {game with world = new_world; player = new_player} in
      print_return new_game;
      game_repl (Some new_game)

    | Save, Some game ->
      Json.save game;
      game_repl gameop

    | Load, _ ->
      let filename = arg in
      let new_game = Json.load filename in
      printf "Game loaded successfully.\n";
      printf "Type 'Enter' to continue game.\n";
      game_repl (Some new_game)

    | New, _ ->
      let filename = Json.new_game() in
      printf "New game created in file %s\n" filename;
      let new_game = Json.load filename in
      printf "Game loaded successfully.\n";
      printf "Type 'Enter' to start game.\n";
      game_repl (Some new_game)

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

(* start the game with no game state *)
(* postcondition: the updated game on exit *)
let enter_game () =
  print_welcome game;
  print_commands();
  ignore (game_repl None)

(* enter_game(); *)