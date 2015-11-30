open Yojson.Basic.Util
open Printf
open Fighter
type ai

type t = {
  id: string;
  unlocked : bool;
  completed : bool;
  opponent: Fighter.t;
  (* ai: ai; *)
  xp: int;
  treasure: Item.t list;
  money: int;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Battles/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let unlocked = json |> member "unlocked" |> to_bool in
  let completed = json |> member "completed" |> to_bool in
  let opponent = json |> member "opponent" |> to_string |> Fighter.from_file path in
  (* let ai = json |> member "ai" |> ai_obj path in *)
  let xp = json |> member "xp" |> to_int in
  let treasure = json |> member "treasure" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let money = json |> member "money" |> to_int in
  {
  id;
  unlocked;
  completed;
  opponent;
  (* ai; *)
  xp;
  treasure;
  money
  }

let to_file path battle =
  failwith "TODO"

exception InvalidBattle of string

let print_battle (b: t) =
  let lockstr = if b.unlocked then "" else " (locked)" in
  printf "%s%s\n" b.id lockstr

let rec str_to_battle bs str =
  match bs with
  | [] -> raise (InvalidBattle str)
  | b::t -> if b.id = str then b else str_to_battle t str

let unlock b =
  {b with unlocked = true}

let get_completed b =
  b.completed
let get_id b =
  b.id

(* The player whose turn it is is first. *)
type state = Fighter.t * Fighter.t


type command =
  | Use
  | Details
  | Exit
  | Equipped
  | Help

type action = | Use of Item.t | Exit

type result = | Win | Lose | Exit

let cmds = ["Use"; "Details"; "Exit"; "Equipped"]

exception InvalidCommand of string

let str_to_command str : command =
  match str with
  | "use" -> Use
  | "details" -> Details
  | "exit" -> Exit
  | "equipped" -> Equipped
  | "Help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "use" -> "Use this item."
  | "details" -> "Display details about this item."
  | "exit" -> "Exit the battle, returning to the zone menu.
  Consumed items are reset and no xp is gained."
  | "equipped" -> "Display the all of the users and opponents equipped items."
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

let print_welcome (battle: t) =
  printf "Welcome to %s\n" battle.id

let rec get_user_action state : action =
  try

    let user = (fst state) in
    let opp = (snd state) in
    Stats.print_battle_stats (Fighter.get_stats user) (Fighter.get_stats opp);
    print_endline "Whats your move?\n";
    let input = String.lowercase (input_line stdin) in
    if String.length input = 0 then raise (InvalidCommand input) else ();
    let inlist = Str.bounded_split (Str.regexp(" ")) (String.lowercase input) 2 in

    let has_arg = List.length inlist > 1 in

    let cmd = str_to_command (List.nth inlist 0) in
    let arg = if has_arg then List.nth inlist 1 else "" in

    match cmd with

    | Help ->
      print_help arg;
      get_user_action state

    | Details ->
      let i = Item.str_to_item (Fighter.get_equipped user) arg in
      printf "%s\n" (Item.get_description i);
      get_user_action state

    | Equipped ->
      Item.print_double_item_list (Fighter.get_equipped user) (Fighter.get_equipped opp);
      get_user_action state

    | Use -> Use(Item.str_to_item (Fighter.get_equipped user) arg)
    | Exit -> Exit

  with
    | InvalidCommand str ->
        printf "\nInvalid command: %s\n" str;
        get_user_action state

    | Item.InvalidItem str ->
        printf "\nInvalid item: %s\n" str;
        get_user_action state

    | Failure str ->
      printf "\nFailure: %s\n" str;
      get_user_action state

let get_ai_action state : action = failwith "unimplemented"

let apply_effect e f : Fighter.t = failwith "unimplemented"

let apply_effects (item: Item.t) f1 f2 : Fighter.t * Fighter.t =
  apply_effect item f1, apply_effect item f2

let remove_item f i = failwith "unimplemented"

let do_action (turn: bool) (state: state) (act: action) : state option =
  let user = (fst state) in
  let opp = (snd state) in
  match act with
  | Use it -> let res = match Item.is_consumable it with
                        | true -> if turn then Some (apply_effects it (remove_item user it) opp)
                                  else let a,b = apply_effects it (remove_item opp it) user in Some (b,a)
                        | false ->  if turn then Some (apply_effects it user opp)
                                    else let a,b = apply_effects it opp user in Some (b,a)
              in res
  | Exit -> None

(* main loop of battle, called once for each turn *)
let rec loop (turn, state) : result * state =
  (* turn being true means it is players turn *)
  let switch turn (a,b) = (not turn), (a,b) in
  let action = (if turn then get_user_action else get_ai_action) state in
  let new_state = do_action turn state action in
  match new_state with
  | None -> (Exit, state)
  | Some (f1, f2) ->  match Fighter.alive f1, Fighter.alive f2 with
                      | true, true -> loop (switch turn (f1, f2))
                      | true, false -> (Win, (f1, f2))
                      | false, true -> (Lose, (f1, f2))
                      | false, false -> failwith "????"

(* Give the player the reward, and update the players inventory *)
let clean_up player fighter battle : Player.t =
  (* TODO *)
  player

let enter_battle battle player : (t * Player.t) =
  print_welcome battle ;
  print_commands ();
  let user = Player.get_fighter player in
  let opp = battle.opponent in
  (* print out player and ai stats, equipped items and commands for battle *)
  Item.print_double_item_list (Fighter.get_equipped user) (Fighter.get_equipped opp);
  match loop (true, (user, opp)) with
  | (Win, (fighter, _)) ->
    let new_battle = {battle with completed = true} in
    let new_player = clean_up player fighter battle in
    printf "Battle won! Returning to zone.";
    (new_battle, new_player)
  | (Lose, (_, _)) ->
    printf "Battle lost! Returning to zone.";
    (battle, player)
  | (Exit, (_, _)) ->
    printf "Battle exited! Returning to zone.";
    (battle, player)