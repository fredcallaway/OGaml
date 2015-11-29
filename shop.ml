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

let print_shop s =
  printf "%s\n" s.id

let to_file path shop =
  failwith "TODO"

let enter_shop (shop: t) (player: Player.t) : (t * Player.t) =
  failwith "TODO"

let buy (item: Item.t) (shop: t) (player: Player.t) : Player.t =
  failwith "TODO"

let sell (item: Item.t) (shop: t) (player: Player.t) : Player.t =
  failwith "TODO"

let equip (item: Item.t) (player: Player.t) : Player.t =
  failwith "TODO"

let remove (item: Item.t) (player: Player.t) : Player.t =
  failwith "TODO"
