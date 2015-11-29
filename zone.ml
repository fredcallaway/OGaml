type t = {
  id : string;
  unlocked : bool;
  completed : bool;
  battles : Battle.t list;
  shop : Shop.t;
}

type command = Enter | Shop | Exit | Map | Score | Help
let cmds = [  "Enter";"Shop";"Exit";"Map";"Score"]
exception InvalidCommand of string
exception InvalidBattle of string

let str_to_command str : command =
  match str with
  | "enter" -> Enter
  | "shop" -> Shop
  | "exit" -> Exit
  | "map" -> Map
  | "score" -> Score
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "enter" -> "Enter the battle."
  | "shop" -> "Enter the shop."
  | "exit" -> "Exit the zone, returning to the world menu."
  | "map" -> "Display the map of the current zone."
  | "score" -> "Display the score in the current zone."
  | _ -> raise (InvalidCommand str)

let print_help (arg: string) =
  let print_helper cmd =
    printf "%s:\n" cmd;
    printf "%s\n\n" (str_to_help cmd);
  in
  if arg = ""
  then List.iter print_helper cmds
  else print_helper arg

let rec str_to_battle bs str =
  match bs with
  | [] -> raise (InvalidBattle str)
  | b::t -> if b.id = str then b else str_to_battle t str

let print_commands () =
  printf "Availible commands: (type 'Help [cmd]' to get more info)\n";
  List.iter (printf "%s\n") (cmds)

let print_welcome (zone: t) =
  printf "Welcome to %s\n" zone.id

let print_return (zone: t) =
  printf "You're now in %s\n" zone.id

let print_map (zone: t) =
  printf "Map:\n";

  printf "Battles:\n";
  let print_battle b =
    let lockstr = if b.unlocked then "" else " (locked)" in
    printf "%s%s\n" b.id lockstr;
  in
  List.iter print_battle zone.battles;

  printf "Shop:\n";
  let print_shop s =
    printf "%s\n" s.id;
  in
  print_shop zone.shop

let print_score (player: Player.t) =
  failwith "TODO"

(* precondition: all battles in zone must have unique ids *)
(* postcondition: zone with updated battle and next battle unlocked if battle was completed *)
let update_battles (zone: t) (battle: Battle.t) : t =
  let rec update_and_unlock (battle_list: Battle.t list) (battle: Battle.t) (a: Battle.t list) =
    match battle_list with
    | [] -> raise (InvalidBattle battle.id)
    | b::t ->
      if b.id = battle.id
      then
        match t with
        | [] ->
          (* if battle.completed *)
          (* then print_win; *)
          a@[battle]@t
        | b_next::t_next ->
          if battle.completed
          then
            let b_next_unlocked = {b_next with unlocked = true} in
            a@[battle;b_next_unlocked]@t_next
          else a@[battle;b_next]@t_next
      else
        update_and_unlock t battle (b::a)
  in
  let new_battles = update_and_unlock zone.battles battle [] in
  {zone with battles = new_battles}


let rec zone_repl (zone: t) (player: Player.t) : (t * Player.t) =
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
    match cmd with

    | Help ->
      print_help arg;
      zone_repl zone player

    | Map ->
      print_map zone;
      zone_repl zone player

    | Score ->
      print_score player;
      zone_repl zone player

    | Enter ->
      let b = str_to_battle zone.battles arg in
      printf "Entering %s\n" arg;
      let new_state = Battle.enter_battle b player in
      let new_battle = (fst new_state) in
      let new_player = (snd new_state) in
      let new_zone = update_battles zone new_battle in
      print_return new_zone;
      zone_repl new_zone new_player

    | Shop ->
      printf "Entering shop\n";
      let new_state = Shop.enter_shop zone.shop player in
      let new_shop = (fst new_state) in
      let new_player = (snd new_state) in
      let new_zone = update_shop zone new_shop in
      print_return new_zone;
      zone_repl new_zone new_player

    | Exit ->
      printf "Exiting %s\n";
      (zone, player)

  with
    | InvalidCommand str ->
      printf "\nInvalid command: %s\n" str;
      zone_repl zone player

    | InvalidBattle str ->
      printf "\nInvalid battle: %s\n" str;
      zone_repl zone player

(* enter the zone with the player *)
(* postcondition: the new player and the updated zone on exit *)
let enter_zone (zone: t) (player: Player.t) : (t * Player.t) =
  print_welcome zone;
  print_commands();
  zone_repl zone player