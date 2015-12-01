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


let buy (id: string) (shop: t) (player: Player.t) : Player.t =
  match (Item.get_item id shop.supply) with
  | None -> player
  | Some i -> if (player.Player.money >= i.Item.value) then 
                let new_money = player.Player.money - i.Item.value in
                let new_inventory = i::player.Player.inventory in 
                {player with inventory = new_inventory; money = new_money}
              else
                player


let sell (id: string) (shop: t) (player: Player.t) : Player.t =
  match (Item.get_item id player.inventory) with
  | None -> player
  | Some i -> let new_money = player.Player.money + (i.Item.value/2) in
              let new_inventory = (Item.remove player.Player.inventory i) in
              {player with inventory = new_inventory; money = new_money} 

let equip (id: string) (player: Player.t) : Player.t =
  match (Item.get_item id player.Player.inventory) with 
  | None -> player
  | Some i -> 
    let old = Item.get_slot_item i player.Player.inventory in 
    begin
      match old with 
      | Some ol -> 
        let new_equipped = i::(Item.remove player.Player.equipped ol) in 
        let new_inventory = ol::(Item.remove player.Player.inventory i) in 
        {player with inventory = new_inventory; equipped = new_equipped}
      | None -> 
        let new_equipped = i::player.Player.equipped in
        let new_inventory = Item.remove player.Player.inventory i in
        {player with inventory = new_inventory; equipped = new_equipped}
    end

let remove (id: string) (player: Player.t) : Player.t =
  match (Item.get_item id player.Player.equipped) with 
  | None -> player
  | Some i -> 
    let new_inventory = i::player.Player.inventory in
    let new_equipped = Item.remove player.Player.equipped i in 
    {player with inventory = new_inventory; equipped = new_equipped}
    

let rec shop_repl (shop: t) (player: Player.t) : (t * Player.t) =
  try
    let cmd, arg = Io.get_input () in 
    let cmd = str_to_command cmd in
    match cmd with

    | Help ->
      shop_repl shop player

    | Score ->
      Player.print_score player;
      shop_repl shop player

    | Buy ->
      let i = Item.str_to_item (Fighter.get_equipped player.fighter) arg in
      let new_player = buy i shop player in
      shop_repl shop new_player

    | Sell ->
      let i = Item.str_to_item (Fighter.get_equipped player.fighter) arg in
      let new_player = sell i shop player in
      shop_repl shop new_player


    | Equip ->
      let i = Item.str_to_item player.equipped arg in
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