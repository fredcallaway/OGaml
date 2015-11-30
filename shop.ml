open Yojson.Basic.Util
open Printf

type t = {
  id: string;
	supply: Item.t list;
}

let from_file path filename =
  (* printf "path/Shops/filename: %s\n" (path^"Shops/"^filename); *)
  let json = Yojson.Basic.from_file (path^"Shops/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let supply = json |> member "supply" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  {
  id;
  supply
  }


type command = Exit | Buy | Sell | Equip | Score | Help
let cmds = [  "Exit";"Buy";"Sell";"Equip";"Score"]
exception InvalidCommand of string

let str_to_command str : command =
  match str with
  | "exit" -> Exit
  | "buy" -> Buy
  | "sell" -> Sell
  | "equip" -> Equip
  | "score" -> Score
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "exit" -> "Exit the shop, returning to the shop menu."
  | "buy" -> "Buy an item."
  | "sell" -> "Sell an item."
  | "equip" -> "Equip an item."
  | "score" -> "Display the score."
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

let print_welcome (shop: t) =
  printf "Welcome to %s\n" shop.id

let print_return (shop: t) =
  printf "You're now in %s\n" shop.id

let print_shop s =
  printf "%s\n" s.id

let to_file path shop =
  failwith "TODO"


let buy (item: Item.t) (shop: t) (player: Player.t) : (t * Player.t) =
  failwith "TODO"

let sell (item: Item.t) (shop: t) (player: Player.t) : (t * Player.t) =
  failwith "TODO"

let equip (item: Item.t) (player: Player.t) : Player.t =
  failwith "TODO"

let remove (item: Item.t) (player: Player.t) : Player.t =
  failwith "TODO"

let enter_shop (shop: t) (player: Player.t) : (t * Player.t) =
  failwith "TODO"

let rec shop_repl (shop: t) (player: Player.t) : (t * Player.t) =
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
      shop_repl shop player

    | Score ->
      Player.print_score player;
      shop_repl shop player

    | Buy ->
      let i = Item.str_to_item (Fighter.get_equipped player.fighter) arg in
      let new_shop, new_player = buy i shop player in
      shop_repl new_shop new_player

    | Sell ->
      let i = Item.str_to_item (Fighter.get_equipped player.fighter) arg in
      let new_shop, new_player = sell i shop player in
      shop_repl new_shop new_player

    | Equip ->
      let i = Item.str_to_item (Fighter.get_equipped player.fighter) arg in
      let new_player = equip i player in
      shop_repl shop new_player

    | Exit ->
      printf "Exiting shop\n";
      (shop, player)

  with
    | InvalidCommand str ->
      printf "\nInvalid command: %s\n" str;
      shop_repl shop player

    | Item.InvalidItem str ->
      printf "\nInvalid item: %s\n" str;
      shop_repl shop player

    | Failure str ->
      printf "\nFailure: %s\n" str;
      shop_repl shop player

(* enter the shop with the player *)
(* postcondition: the new player and the updated shop on exit *)
let enter_shop (shop: t) (player: Player.t) : (t * Player.t) =
  print_welcome shop;
  print_commands();
  shop_repl shop player