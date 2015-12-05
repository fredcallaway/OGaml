open Yojson.Basic.Util
open Printf

type t = {
  id: string;
	supply: Item.t list;
}

let from_file path filename =
  let json = Yojson.Basic.from_file (path^"Shops/"^filename) in
  let id = String.sub filename 0 (String.length filename - 5) in
  let supply = json |> member "supply" |> to_list |> List.map to_string |> List.map (Item.from_file path) in
  {
  id;
  supply
  }

let to_file path shop =
  let supply_json = `List (shop.supply |> List.map (Item.to_file path)) in
  let shop_json = `Assoc [
    ("supply", supply_json)
  ] in
  let filename = shop.id^".json" in
  Yojson.Basic.to_file (path^"Shops/"^filename) shop_json;
  `String filename


type command = Supply | Buy | Sell | Equip | Remove | Bag | Help
let cmds = [  "Exit";"Supply";"Buy";"Sell";"Equip";"Remove";"Bag"; "Help"]
exception InvalidCommand of string

let str_to_command str : command =
  match str with
  | "supply" -> Supply
  | "buy" -> Buy
  | "sell" -> Sell
  | "equip" -> Equip
  | "remove" -> Remove
  | "bag" -> Bag
  | "help" -> Help
  | _ -> raise (InvalidCommand str)

let str_to_help str : string =
  match String.lowercase str with
  | "exit" -> "Exit the shop, returning to the shop menu."
  | "supply" -> "View shop's supply."
  | "buy" -> "Buy an item."
  | "sell" -> "Sell an item."
  | "equip" -> "Equip an item."
  | "remove" -> "Remove an equipped item."
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

let print_welcome (shop: t) =
  printf "Welcome to %s\n" shop.id

let print_return (shop: t) =
  printf "You're now in %s\n" shop.id

let print_shop s =
  printf "%s\n" s.id

let print_supply (s: t) =
  printf "Shop's Supply:\n";
  Player.print_item_list (s.supply)

let buy (id: string) (shop: t) (player: Player.t) : Player.t =
  match (Item.get_item id shop.supply) with
  | None -> printf "Item %s not in shop.\n" id; player
  | Some i -> if (player.Player.money >= i.Item.value) then
                let () = printf "Bought %s for %d!\n" id i.Item.value in
                let new_money = player.Player.money - i.Item.value in
                let new_inventory = i::player.Player.inventory in
                {player with Player.inventory = new_inventory; Player.money = new_money}
              else
                (printf "Not enough money! Need %d. Have %d.\n" i.Item.value player.Player.money; player)


let sell (id: string) (shop: t) (player: Player.t) : Player.t =
  match (Item.get_item id player.Player.inventory) with
  | None -> printf "Item %s not in inventory.\n" id; player
  | Some i -> printf "Sold %s for %d!\n" id (i.Item.value/2);
              let new_money = player.Player.money + (i.Item.value/2) in
              let new_inventory = (Item.remove player.Player.inventory i) in
              {player with Player.inventory = new_inventory; Player.money = new_money}

let equip (id: string) (player: Player.t) : Player.t =
  match (Item.get_item id player.Player.inventory) with
  | None -> printf "Item %s not in inventory.\n" id; player
  | Some i -> 
    let old = Item.get_slot_item i player.Player.equipped in 
    begin
      match old with
      | Some ol ->
        printf "Equipped %s for %s!\n" id ol.Item.id;
        let new_equipped = (Item.remove player.Player.equipped ol @ [i]) in
        let new_inventory = ol::(Item.remove player.Player.inventory i) in
        {player with Player.inventory = new_inventory; Player.equipped = new_equipped}
      | None ->
        printf "Equipped %s for blank slot!\n" id;
        let new_equipped = (player.Player.equipped @ [i]) in
        let new_inventory = Item.remove player.Player.inventory i in
        {player with Player.inventory = new_inventory; Player.equipped = new_equipped}
    end

let remove (id: string) (player: Player.t) : Player.t =
  match (Item.get_item id player.Player.equipped) with
  | None -> printf "Item %s is not equipped!\n" id; player
  | Some i ->
    printf "Removed %s from equipped.\n" id;
    let new_inventory = i::player.Player.inventory in
    let new_equipped = Item.remove player.Player.equipped i in
    {player with Player.inventory = new_inventory; Player.equipped = new_equipped}

let rec shop_repl (shop: t) (player: Player.t) : (t * Player.t) =
  try
    let cmd, arg = Io.get_input () in
    let cmd = str_to_command cmd in
    match cmd with

    | Help ->
      print_welcome shop;
      print_commands();
      shop_repl shop player

    | Supply ->
      print_supply shop;
      shop_repl shop player

    | Bag ->
      Player.print_bag player;
      shop_repl shop player

    | Buy ->
      let i =  arg in
      let new_player = buy i shop player in
      shop_repl shop new_player

    | Sell ->
      let i = arg in
      let new_player = sell i shop player in
      shop_repl shop new_player

    | Equip ->
      let i = arg in
      let new_player = equip i player in
      shop_repl shop new_player

    | Remove ->
      let i = arg in
      let new_player = remove i player in
      shop_repl shop new_player


  with
    | InvalidCommand str ->
      printf "\nInvalid command: %s\n" str;
      shop_repl shop player

    | Item.InvalidItem str ->
      printf "\nInvalid item: %s\n" str;
      shop_repl shop player

    | Exit ->
      printf "Exiting shop\n";
      (shop, player)

(* enter the shop with the player *)
(* postcondition: the new player and the updated shop on exit *)
let enter_shop (shop: t) (player: Player.t) : (t * Player.t) =
  print_welcome shop;
  print_commands();
  shop_repl shop player