type t = {
  id : string;
  completed : bool;
  zones : Zone.t list;
}

type command = Enter | Exit | Map | Score | Help
let cmds = [  "Enter";"Exit";"Map";"Score"]
exception InvalidCommand of string
exception InvalidZone of string

let str_to_command str : command =
  match str with
  | "enter" -> Enter
  | "exit" -> Exit
  | "map" -> Map
  | "score" -> Score
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "enter" -> "Enter the zone."
  | "exit" -> "Exit the world, returning to the main menu."
  | "map" -> "Display the map of the current world."
  | "score" -> "Display the score in the current world."
  | _ -> raise (InvalidCommand str)

let print_help (arg: string) =
  let print_helper cmd =
    printf "%s:\n" cmd;
    printf "%s\n\n" (str_to_help cmd);
  in
  if arg = ""
  then List.iter print_helper cmds
  else print_helper arg

let rec str_to_zone zs str =
  match zs with
  | [] -> raise (InvalidZone str)
  | z::t -> if z.id = str then z else str_to_zone t str

let print_commands () =
  printf "Availible commands: (type 'Help [cmd]' to get more info)\n";
  List.iter (printf "%s\n") (cmds)

let print_welcome (world: t) =
  printf "Welcome to %s\n" world.id

let print_return (world: t) =
  printf "You're now in %s\n" world.id

let print_map (world: t) =
  printf "Map:\n";
  let print_zone z =
    let lockstr = if z.unlocked then "" else " (locked)" in
    printf "%s%s\n" z.id lockstr;
  in List.iter (print_zone) (world.zones)

let print_score (player: Player.t) =
  failwith "TODO"

(* precondition: all zones in world must have unique ids *)
(* postcondition: world with updated zone and next zone unlocked if zone was completed *)
let update_zones (world: t) (zone: Zone.t) : t =
  let rec update_and_unlock (zone_list: Zone.t list) (zone: Zone.t) (a: Zone.t list) =
    match zone_list with
    | [] -> raise (InvalidZone zone.id)
    | z::t ->
      if z.id = zone.id
      then
        match t with
        | [] ->
          (* if zone.completed *)
          (* then print_win; *)
          a@[zone]@t
        | z_next::t_next ->
          if zone.completed
          then
            let z_next_unlocked = {z_next with unlocked = true} in
            a@[zone;z_next_unlocked]@t_next
          else a@[zone;z_next]@t_next
      else
        update_and_unlock t zone (z::a)
  in
  let new_zones = update_and_unlock world.zones zone [] in
  {world with zones = new_zones}


let rec world_repl (world: t) (player: Player.t) : (t * Player.t) =
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
      world_repl world player

    | Map ->
      print_map world;
      world_repl world player

    | Score ->
      print_score player;
      world_repl world player

    | Enter ->
      let z = str_to_zone world.zones arg in
      printf "Entering %s\n" arg;
      let new_state = Zone.enter_zone z player in
      let new_zone = (fst new_state) in
      let new_player = (snd new_state) in
      let new_world = update_zones world new_zone in
      print_return new_world;
      world_repl new_world new_player

    | Exit ->
      printf "Exiting\n";
      (world, player)

  with
    | InvalidCommand str ->
      printf "\nInvalid command: %s\n" str;
      world_repl world player

    | InvalidZone str ->
      printf "\nInvalid zone: %s\n" str;
      world_repl world player

(* enter the world with the player *)
(* postcondition: the new player and the updated world on exit *)
let enter_world (world: t) (player: Player.t) : (t * Player.t) =
  print_welcome world;
  print_commands();
  world_repl world player