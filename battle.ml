open Yojson.Basic.Util
open Printf
open Fighter


type t = {
  id: string;
  unlocked : bool;
  completed : bool;
  opponent: Fighter.t;
  treasure: Item.t list;
  money: int;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Battles/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let unlocked = json |> member "unlocked" |> to_bool in
  let completed = json |> member "completed" |> to_bool in
  let opponent = json |> member "opponent" |> to_string |> Fighter.from_file path in
  let treasure = json |> member "treasure" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  let money = json |> member "money" |> to_int in
  {
  id;
  unlocked;
  completed;
  opponent;
  (* ai; *)
  treasure;
  money
  }

let to_file path battle =
  let unlocked_json = `Bool (battle.unlocked) in
  let completed_json = `Bool (battle.completed) in
  let opponent_json = battle.opponent |> Fighter.to_file path in
  (* let ai_json = battle.ai |> Ai.to_file path in *)
  let treasure_json = `List (battle.treasure |> List.map (Item.to_file path)) in
  let money_json = `Int (battle.money) in
  let battle_json = `Assoc [
    ("unlocked", unlocked_json);
    ("completed", completed_json);
    ("opponent", opponent_json);
    (* ("ai", ai_json); *)
    ("treasure", treasure_json);
    ("money", money_json)
  ] in
  let filename = battle.id^".json" in
  Yojson.Basic.to_file (path^"Battles/"^filename) battle_json;
  `String filename

exception InvalidBattle of string

let battle_to_string (b: t) =
  let lockstr = if b.unlocked then "" else " (locked)" in
  b.id^lockstr

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
let get_unlocked b =
  b.unlocked

(* User is always first, ai always second *)
type state = Fighter.t * Fighter.t

(* items self effects are always applied to f1, thus the user of the item
 * should always be f1. *)
let apply_effects (item: Item.t) (state: state) : Fighter.t * Fighter.t =
  let self, opp = state in
  let self_stats',opp_stats' =
    Stats.apply (Item.get_resistance_effect item)
    (Item.get_self_effect item) (Item.get_opponent_effect item)
    (Fighter.get_stats self) (Fighter.get_stats opp) in
  let self' = Fighter.set_stats self self_stats' in
  let opp' = Fighter.set_stats opp opp_stats' in
  (self', opp')

let remove_item f i =
  let new_equipped = Item.remove (Fighter.get_equipped f) i in
  Fighter.set_equipped f new_equipped

(* The state resulting from the active fighter using
 * the given item. The active fighter of state (f1, f2)
 * is f1 when turn is true. *)
let use_item (turn: bool) (state: state) (item: Item.t) : state =
  let switch (a,b) = (b,a) in
  let user, opp = state in
  match turn, Item.is_consumable item with
  | true, true ->   apply_effects item ((remove_item user item), opp)
  | true, false ->  apply_effects item state
  | false, true ->  apply_effects item ((remove_item opp item), user) |> switch
  | false, false -> apply_effects item (switch state) |> switch


(******************
 * USER INTERFACE *
 ******************)
let cmds = ["Use"; "Details"; "Exit"; "Equipped"]

let str_to_help str : string =
  match String.lowercase str with
  | "use" -> "Use this item."
  | "details" -> "Display details about this item."
  | "exit" -> "Exit the battle, returning to the zone menu.\n" ^
              "Consumed items are reset and no xp is gained."
  | "equipped" -> "Display the all of the users and opponents equipped items."
  | _ -> "Invalid command."

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
    match cmd with
    | "help" ->
      print_help arg;
      get_user_action state

    | "details" ->
      let i = Item.str_to_item (Fighter.get_equipped user) arg in
      printf "%s\n" (Item.get_description i);
      get_user_action state

    | "equipped" ->
      Item.print_double_item_list (Fighter.get_equipped user) (Fighter.get_equipped opp);
      get_user_action state

    | "use" ->
      let item = Item.str_to_item (Fighter.get_equipped user) arg in
      printf "User used %s!\n" item.Item.id; item

    | item_id -> 
      let item = Item.str_to_item (Fighter.get_equipped user) item_id in
      printf "User used %s!\n" item.Item.id; item
      (* print_endline "Invalid command\n"; get_user_action state *)

  with
    | Item.InvalidItem str ->
        printf "\nInvalid item: %s\n" str;
        get_user_action state


(******
 * AI *
 ******)

(* how much better off the active fighter is *)
let ai_value_heuristic turn (f1, f2) : float =
  let self, opp = if turn then f1, f2 else f2, f1 in
  match (Fighter.alive self), (Fighter.alive opp) with
  | false, _ -> -. max_float
  | true, false -> max_float
  | true, true-> 
    Stats.difference (Fighter.get_stats self) 
                     (Fighter.get_stats opp)

(* the value of a state for the active fighter *)
let rec ai_value depth turn  (state: state) : float =
  (* printf "ai_value %b %i\n" turn depth; *)
  let ai = (if turn then fst else snd) state in

  let result =
  match depth with
  | 0 -> ai_value_heuristic turn state
  | _ ->
    let child_states = List.map (use_item turn state) (Fighter.get_equipped ai) in
    let child_ai = (ai_value (depth-1)) (not turn) in
    let child_values = List.map child_ai child_states in
    Utils.list_max (~+.) (List.map (~-.) child_values)

  in result

(* An item that is deemed appropriate to use in a given state.
 * Turn indicates whether ai is *)
let get_ai_action turn depth state : Item.t =
  let ai = (if turn then fst else snd) state in
  let child_ai = (ai_value (depth)) (not turn) in
  Fighter.get_equipped ai
  |> Utils.list_max (fun it -> (use_item turn state it)
                                |> child_ai
                                |> (~-.))

let random_action turn state : Item.t =
  let ai = (if turn then fst else snd) state in
  let choices = Fighter.get_equipped ai in
  List.nth choices (Random.int (List.length choices))

(* Returns a random action with probability [fuzz], otherwise
 * returns an "intelligent" action. *)
let get_fuzzy_ai_action fuzz depth turn state : Item.t = 
  let item = 
    if Random.float 1.0 > fuzz
    then get_ai_action turn depth state
    else random_action turn state
  in printf "Opponent used %s!\n" item.Item.id; item


(********
 * MAIN *
 ********)

type result = | Win | Lose

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
let give_reward player fighter battle : Player.t =
  {player with Player.money = player.Player.money + battle.money;
               Player.inventory = player.Player.inventory @ battle.treasure;}

let enter_battle battle player : (t * Player.t) =
  print_welcome battle ;
  print_commands ();
  let user = Fighter.apply_base_effects (Fighter.make player) in
  let opp = Fighter.apply_base_effects battle.opponent in
  (* print out player and ai stats, equipped items and commands for battle *)
  Item.print_double_item_list (Fighter.get_equipped user) (Fighter.get_equipped opp);
  try
    let ai = (get_fuzzy_ai_action 0.3 3 false) in
    match run_battle (user, opp) get_user_action ai with
    | (Win, (fighter, _)) ->
      let new_battle = {battle with completed = true} in
      let new_player = give_reward player fighter battle in
      printf "Battle won! Returning to zone.";
      (new_battle, new_player)
    | (Lose, (_, _)) ->
      printf "Battle lost! Returning to zone.";
      (battle, player)
  with
   | Io.Exit -> print_endline "Battle exited! Returning to zone."; (battle, player)



