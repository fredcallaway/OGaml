open Yojson.Basic.Util

(* initializes the game with the given json filename *)
let load path filename =

  let item_obj path filename =
    let json = Yojson.Basic.from_file (path^"Items/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _description = json |> member "description" |> to_string in
    let _value = json |> member "points" |> to_int in
    let _slot = json |> member "slot" |> to_string in
    {
    id=_id;
    description=_description;
    value=_value;
    slot=_slot;
    }
  in

  let stats_obj path filename =
    let json = Yojson.Basic.from_file (path^"Stats/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _health = json |> member "health" |> to_int in
    let _strength = json |> member "strength" |> to_int in
    let _speed = json |> member "speed" |> to_int in
    let _dexterity = json |> member "dexterity" |> to_int in
    let _magic = json |> member "magic" |> to_int in
    {
    id=_id;
    health=_health;
    strength=_strength;
    speed=_speed;
    dexterity=_dexterity;
    magic=_magic
    }
  in

  let fighter_obj path filename =
    let json = Yojson.Basic.from_file (path^"Fighters/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _stats = json |> member "stats" |> to_string |> stats_obj path in
    let _inventory = json |> member "inventory" |> to_list |> List.map to_string |> List.map (item_obj path) in
    let _equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (item_obj path) in
    let _hp = json |> member "hp" |> to_float in
    (* let _effects = json |> member "effects" |> to_list |> List.map effect_obj path in *)
    {
    id=_id;
    stats=_stats;
    inventory=_inventory;
    equipped=_equipped;
    hp=_hp
    (* effects=_effects *)
    }
  in

  let battle_obj path filename =
    let json = Yojson.Basic.from_file (path^"Battles/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _opponent = json |> member "opponent" |> fighter_obj path in
    (* let _ai = json |> member "ai" |> ai_obj path in *)
    let _xp = json |> member "xp" |> to_int in
    let _treasure = json |> member "treasure" |> to_list |> List.map to_string |> List.map (item_obj path) in
    let _money = json |> member "money" |> to_int in
    {
    id=_id;
    opponent=_opponent;
    (* ai=_ai; *)
    xp=_xp;
    treasure=_treasure;
    money=_money;
    }
  in

  let shop_obj path filename =
    let json = Yojson.Basic.from_file (path^"Shops/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _supply = json |> member "supply" |> to_list |> List.map to_string |> List.map (item_obj path) in
    {
    id=_id;
    supply=_supply
    }
  in

  let player_obj path filename =
    let json = Yojson.Basic.from_file (path^"Players/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _equipped = json |> member "equipped" |> to_list |> List.map to_string |> List.map (item_obj path) in
    let _money = json |> member "money" |> to_int in
    let _expereience = json |> member "expereience" |> to_int in
    let _level = json |> member "level" |> to_int in
    {
    id=_id;
    money=_money;
    expereience=_expereience;
    level=_level
    }
  in

  let zone_obj path filename =
    let json = Yojson.Basic.from_file (path^"Zones/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _unlocked = json |> member "unlocked" |> to_bool in
    let _completed = json |> member "completed" |> to_bool in
    let _battles = json |> member "battles" |> to_list |> List.map to_string |> List.map (battle_obj path) in
    let _shop = json |> member "shop" |> to_string |> shop_obj path in
    {
    id=_id;
    unlocked=_unlocked;
    completed=_completed;
    battles=_battles;
    shop=_shop
    }
  in

  let world_obj path filename =
    let json = Yojson.Basic.from_file (path^"Worlds/"^filename) in
    let _id = json |> member "id" |> to_string in
    let _completed = json |> member "completed" |> to_bool in
    let _zones = json |> member "zones" |> to_list |> List.map to_string |> List.map (zone_obj path) in
    {
    id=_id;
    completed=_completed;
    zones=_zones;
    }
  in

  let game_obj path filename =
    let json = Yojson.Basic.from_file (path^filename) in
    (* YYojson.Basic.pretty_to_string json; *)
    let _id = json |> member "id" |> to_string in
    let _world = json |> member "world" |> to_string |> world_obj path in
    let _player = json |> member "player" |> to_string |> player_obj path in
    {
    id=_id;
    world=_world;
    player=_player;
    }
  in

  game_obj path filename

(* save the game state into a json file *)
let save game : unit =
  failwith "TODO"

(* create a new json file for a new game *)
let new_game path filename : string =
  failwith "TODO"
