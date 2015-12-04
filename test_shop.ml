open Shop
open Assertions

let testdir = "SavedGames/game1/"

let bow = Item.from_file testdir "Bow.json"
let body_armor = Item.from_file testdir "Body Armor.json"
let health_potion = Item.from_file testdir "Health Potion.json"
let helmet = Item.from_file testdir "Helmet.json"
let knife = Item.from_file testdir "Knife.json"
let sword = Item.from_file testdir "Sword.json"

(* Player has health potion, sword, no money *)
let p1 = Player.from_file testdir "player0.json"
let items = [bow; body_armor; health_potion; helmet; knife; sword]
let test_shop = {id= "Test Shop"; supply= items}


let () = 
	let x = enter_shop test_shop p1 in 
	if (fst x).id = "Test Shop" then ()
	else () 

(* THINGS TO TEST
BUY
	buy with not enough money -> print cannot; loop back
	buy with exactly enough money -> print bought; add to inventory; subtract money; stays in shop
	buy item already have -> print bought; add to inventory; subtract money
	buy nonexistent item -> print not there; loop back
SELL
	sell item you dont have -> print cannot; loop back
	sell item you have -> print sold; add money; remove from inventory
	sell item you have two of -> print sold; add money; only one in inventory
	sell item equipped -> print cant; loop back 
EQUIP
	equip item you dont have -> print cannot; loop back
	equip item into empty slot -> print done; remove new from inventory; add new to equipped
	equip item into nonempty slot -> print dont; remove new from inventory; add new to equipped; remove old from equipped; add old to inventory
	equip consumable into empty slot -> print done; remove new from inventroy; add new to equipped
	equip consumable to full slot -> print done; remove first consumable from equipped and add to inventory; add new consumable to end of equipped
REMOVE
	remove item not equipped -> print cannot; loop back
	remove item that is equipped -> print done; remove from equipped; add to inventory
 *)
