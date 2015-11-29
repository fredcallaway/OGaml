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


type action =
  | Consume of Item.t
  | Use of Item.t

let get_user_action state : action = failwith "unimplemented"
let get_ai_action state : action = failwith "unimplemented"

let apply_effect e f : Fighter.t = failwith "unimplemented"

let apply_effects (e1, e2) (f1, f2) : state =
  apply_effect e1 f1, apply_effect e2 f2

let remove_item f i = failwith "unimplemented"

let do_action (state: state) (act: action) : state =
    match act with
    | Consume item -> begin
      let f1, f2 = state in
      let state' = remove_item f1 item, f2 in
      apply_effects (Item.get_effects item) state'
    end
    | Use item -> begin
      apply_effects (Item.get_effects item) state
    end

(* main loop of battle, called once for each turn *)
let rec loop (turn, state) : bool * state =
  (* turn being true means it is players turn *)
  let switch turn (a,b) = (not turn), (b,a) in
  let action = (if turn then get_user_action else get_ai_action) state in
  let state = do_action state action in
  let f1, f2 = state in
  match alive f1, alive f2 with
  | true, true -> loop (switch turn state)
  | true, false -> turn, state
  | false, true -> switch turn state
  | false, false -> failwith "????"

(* Give the player the reward, and update the players inventory *)
let clean_up player fighter battle : Player.t =
  (* TODO *)
  player

let enter_battle battle player : (t * Player.t) =
  match loop (true, (player.Player.fighter, battle.opponent)) with
  | (true, (fighter, _)) ->
    let new_battle = {battle with completed = true} in
    let new_player = clean_up player fighter battle in
    printf "Battle won! Returning to zone.";
    (new_battle, new_player)
  | (false, (_, _)) ->
    printf "Battle lost! Returning to zone.";
    (battle, player)