open Yojson.Basic.Util
open Printf

type t = {
  id : string;
  completed : bool;
  zones : Zone.t list;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Worlds/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let completed = json |> member "completed" |> to_bool in
  let zones = json |> member "zones" |> to_list |> List.map to_string |> List.map (Zone.from_file path) in
  {
  id;
  completed;
  zones
  }

let to_file path world =
  let completed_json = `Bool (world.completed) in
  let zones_json = `List (world.zones |> List.map (Zone.to_file path)) in
  let world_json = `Assoc [
    ("completed", completed_json);
    ("zones", zones_json)
  ] in
  let filename = world.id^".json" in
  Yojson.Basic.to_file (path^"Worlds/"^filename) world_json;
  `String filename

type command = Enter | Exit | Map | Bag | Help
let cmds = [  "Enter";"Exit";"Map";"Bag"]
exception InvalidCommand of string

let str_to_command str : command =
  match str with
  | "enter" -> Enter
  | "exit" -> Exit
  | "map" -> Map
  | "bag" -> Bag
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "enter" -> "Enter the zone."
  | "exit" -> "Exit the world, returning to the main menu."
  | "map" -> "Display the map of the current world."
  | "bag" -> "Display money, inventory, and equipped."
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
  List.iter (printf "%s\n") (cmds);
  printf "\n"

let print_welcome (world: t) =
  printf "Welcome to %s\n" world.id

let print_return (world: t) =
  printf "You're now in %s\n" world.id

let print_map (world: t) =
  printf "Map:\n";
  List.iter (Zone.print_zone) (world.zones);
  printf "\n"

(* precondition: all zones in world must have unique ids *)
(* postcondition: world with updated zone and next zone unlocked if zone was completed *)
let update_zones (world: t) (zone: Zone.t) : t =
  let rec update_and_unlock (zone_list: Zone.t list) (zone: Zone.t) (a: Zone.t list) =
    match zone_list with
    | [] -> raise (Zone.InvalidZone (Zone.get_id zone))
    | z::t ->
      if Zone.get_id z = Zone.get_id zone
      then
        match t with
        | [] ->
          (* if zone.completed *)
          (* then print_win; *)
          a@[zone]@t
        | z_next::t_next ->
          if Zone.get_completed zone
          then
            let z_next_unlocked = Zone.unlock z_next in
            a@[zone;z_next_unlocked]@t_next
          else a@[zone;z_next]@t_next
      else
        update_and_unlock t zone (z::a)
  in
  let new_zones = update_and_unlock world.zones zone [] in
  {world with zones = new_zones}


let rec world_repl (world: t) (player: Player.t) : (t * Player.t) =
  try
    let cmd, arg = Io.get_input () in
    match str_to_command cmd with

    | Help ->
      print_help arg;
      world_repl world player

    | Map ->
      print_map world;
      world_repl world player

    | Bag ->
      Player.print_bag player;
      world_repl world player

    | Enter ->
      let z = Zone.str_to_zone world.zones arg in
      if not (Zone.get_unlocked z)
      then raise (Zone.InvalidZone ((Zone.get_id z)^" locked."))
      else
        printf "Entering %s\n\n" arg;
        let new_state = Zone.enter_zone z player in
        let new_zone = (fst new_state) in
        let new_player = (snd new_state) in
        let new_world = update_zones world new_zone in
        print_return new_world;
        world_repl new_world new_player

    | Exit ->
      printf "Exiting world\n";
      (world, player)

  with
    | InvalidCommand str ->
      printf "\nInvalid command: %s\n" str;
      world_repl world player

    | Zone.InvalidZone str ->
      printf "\nInvalid zone: %s\n" str;
      world_repl world player

    | Failure str ->
      printf "\nFailure: %s\n" str;
      world_repl world player

(* enter the world with the player *)
(* postcondition: the new player and the updated world on exit *)
let enter_world (world: t) (player: Player.t) : (t * Player.t) =
  print_welcome world;
  print_commands();
  print_map world;
  world_repl world player