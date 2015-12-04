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
  (* let ai = json |> member "ai" |> Ai.to_file path in *)
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
  let unlocked_json = `Bool (battle.unlocked) in
  let completed_json = `Bool (battle.completed) in
  let opponent_json = battle.opponent |> Fighter.to_file path in
  (* let ai_json = battle.ai |> Ai.to_file path in *)
  let xp_json = `Int (battle.xp) in
  let treasure_json = `List (battle.treasure |> List.map (Item.to_file path)) in
  let money_json = `Int (battle.money) in
  let battle_json = `Assoc [
    ("unlocked", unlocked_json);
    ("completed", completed_json);
    ("opponent", opponent_json);
    (* ("ai", ai_json); *)
    ("xp", xp_json);
    ("treasure", treasure_json);
    ("money", money_json)
  ] in
  let filename = battle.id^".json" in
  Yojson.Basic.to_file (path^"Battles/"^filename) battle_json;
  `String filename

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

(* User is always first, ai always second *)
type state = Fighter.t * Fighter.t


type command =
  | Use
  | Details
  | Equipped
  | Help

type result = | Win | Lose | Exit



(* items self effects are always applied to f1, thus the user of the item
 * should always be f1. *)
let apply_effects (item: Item.t) f1 f2 : Fighter.t * Fighter.t =
  (* TOOD: Polosky
   * make these functions smarter. *)
  let self_effect it f =
    Fighter.set_stats f (Stats.combine (Item.get_self_effect it)
                                       (Fighter.get_stats f))
  in
  let opponent_effect it f1 f2 =
    Fighter.set_stats f2 (Stats.combine (Item.get_opponent_effect it)
                                        (Fighter.get_stats f2))
  in
  let new_f1 = self_effect item f1 in
  let new_f2 = opponent_effect item f1 f2 in
  (new_f1, new_f2)

let remove_item f i =
  let new_equipped = Item.remove (Fighter.get_equipped f) i in
  Fighter.set_equipped f new_equipped


let use_item (turn: bool) (state: state) (it: Item.t) : state =
let switch (a,b) = (b,a) in
  let user, opp = state in
  match turn, Item.is_consumable it with
  | true, true -> apply_effects it (remove_item user it) opp
  | true, false ->  apply_effects it user opp
  | false, true -> apply_effects it (remove_item opp it) user |> switch
  | false, false ->  apply_effects it opp user |> switch



let cmds = ["Use"; "Details"; "Exit"; "Equipped"]

exception InvalidCommand of string

let str_to_command str : command =
  match str with
  | "use" -> Use
  | "details" -> Details
  | "equipped" -> Equipped
  | "help" -> Help
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

let rec get_user_action state : Item.t =
  try

    let user, opp = state in
    let cmd, arg = Io.get_input () in
    match str_to_command cmd with
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

    | Use ->
      printf "User used %s!\n" arg;
      Item.str_to_item (Fighter.get_equipped user) arg

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

(* how much better off the ai is *)
let ai_value_heuristic turn (f1, f2) : int =
  let self, opp = if turn then f1, f2 else f2, f1 in
  Stats.difference (Fighter.get_stats self) (Fighter.get_stats opp)


(* the value of a state for the fighter whose turn it is *)
let rec ai_value turn depth (state: state) : int =
  let ai = (if turn then fst else snd) state in
  match depth with
  | 0 -> ai_value_heuristic turn state
  | _ ->
    let child_states = List.map (use_item false state) (Fighter.get_equipped ai) in
    let child_ai = (ai_value (not turn) (depth-1)) in
    let child_values = List.map child_ai child_states in
    Utils.list_max (~-) child_values

let get_ai_action turn depth state : Item.t =
  let ai = (if turn then fst else snd) state in
  let child_ai_value = (ai_value (not turn) (depth-1)) in
  Fighter.get_equipped ai
  |> Utils.list_max (fun it -> (use_item false state it)
                               |> child_ai_value
                               |> (~-))

(*
  let result act = do_action false state act) options in
  Utils.list_max (get_value @@ do_action false state)

  printf "Opponent used %s!\n" (Item.get_id (List.hd ai_equipped));
  Use (List.hd ai_equipped)
 *)


let run_battle init_state get_p1_action get_p2_action : result * state =
  (* main loop of battle, called once for each turn *)
  let rec loop turn state : result * state =
    let user, opp = state in
    Stats.print_battle_stats (Fighter.get_stats user) (Fighter.get_stats opp);
    (* turn being true means it is players turn *)
    let item = (if turn then get_p1_action else get_p2_action) state in
    let f1, f2 = use_item turn state item in
    match Fighter.alive f1, Fighter.alive f2 with
    | true, true -> loop (not turn) (f1, f2)
    | true, false -> (Win, (f1, f2))
    | false, true -> (Lose, (f1, f2))
    | false, false -> failwith "????"

  in loop true init_state

(* Give the player the reward, and update the players inventory *)
let clean_up player fighter battle : Player.t =
  (* TODO *)
  player

let enter_battle battle player : (t * Player.t) =
  print_welcome battle ;
  print_commands ();
  let user = Fighter.make player in
  let opp = battle.opponent in
  (* print out player and ai stats, equipped items and commands for battle *)
  Item.print_double_item_list (Fighter.get_equipped user) (Fighter.get_equipped opp);
  match run_battle (user, opp) get_user_action (get_ai_action true 3) with
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