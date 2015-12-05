open Shop
open Assertions

let testdir = "SavedGames/game1/"

let bow = Item.from_file testdir "Bow.json"
let body_armor = Item.from_file testdir "Body Armor.json"
let health_potion = Item.from_file testdir "Health Potion.json"
let dexterity_potion = Item.from_file testdir "Dexterity Potion.json"
let helmet = Item.from_file testdir "Helmet.json"
let knife = Item.from_file testdir "Knife.json"
let sword = Item.from_file testdir "Sword.json"

(* Player has health potion, sword, no money *)
let p1 = Player.from_file testdir "player0.json"
let items = [bow; body_armor; health_potion; helmet; knife; sword; dexterity_potion]
let test_shop = {id= "Test Shop"; supply= items}

let test_buy = 
	(* buy with not enough money -> print cannot; loop back *)
	let p1 = buy "bow" test_shop p1 in 
	(List.mem bow p1.Player.inventory) === false;
	(* buy with exactly enough money -> print bought; add to inventory; 
	subtract money; stays in shop *)
	let p1 = buy "bow" test_shop {p1 with Player.money = 40} in
	(List.mem bow p1.Player.inventory) === true;
	(List.mem bow test_shop.supply) === true;
	p1.Player.money === 0;
	(* buy item already have -> print bought; add to inventory; subtract money *)
	let p1 = buy "bow" test_shop {p1 with Player.money = 40} in
	(List.mem bow (Item.remove p1.Player.inventory bow)) === true;
	(List.mem bow (Item.remove (Item.remove p1.Player.inventory bow) bow)) === false;
	p1.Player.money === 0;
	(* buy nonexistent item -> print not there; loop back *)
	let p1 = buy "oboe" test_shop {p1 with Player.money = 40} in 
	p1.Player.money === 40;
	()

let test_sell = 
	let p1 = buy "bow" test_shop {p1 with Player.money = 40} in
	(* sell item you dont have -> print cannot; loop back *)
	let p1 = sell "oboe" test_shop p1 in 
	p1.Player.money === 0;
	(* sell item you have -> print sold; add money; remove from inventory *)
	let p1 = sell "bow" test_shop p1 in 
	p1.Player.money === 20;
	(List.mem bow p1.Player.inventory) === false;
	(* sell item you have two of -> print sold; 
	add money; only one in inventory *)
	let p1 = buy "bow" test_shop {p1 with Player.money = 80} in
	let p1 = buy "bow" test_shop p1 in
	let p1 = sell "bow" test_shop p1 in 
	p1.Player.money === 20;
	(List.mem bow p1.Player.inventory) === true;
	(List.mem bow (Item.remove p1.Player.inventory bow)) === false;
	(* sell item equipped -> print cant; loop back  *)
	let p1 = sell "sword" test_shop p1 in 
	p1.Player.money === 20;
	(List.mem sword p1.Player.equipped) === true;
	()

let test_equip = 
	let p1 = buy "helmet" test_shop {p1 with Player.money = 500} in 
	let p1 = buy "bow" test_shop p1 in 
	let p1 = buy "health potion" test_shop p1 in 
	let p1 = buy "health potion" test_shop p1 in 
	let p1 = buy "dexterity potion" test_shop p1 in 
	(* equip item you dont have -> print cannot; loop back *)
	let p1 = equip "oboe" p1 in 
	List.length p1.Player.equipped === 1;
	(* equip item into empty slot -> print done; remove new from inventory; 
	add new to equipped *)
	let p1 = equip "helmet" p1 in 
	(List.mem helmet p1.Player.inventory) === false;
	(List.mem helmet p1.Player.equipped) === true;
	(* equip item into nonempty slot -> print done; remove new from inventory; 
	add new to equipped; remove old from equipped; add old to inventory *)
    let p1 = equip "bow" p1 in 
    (List.mem bow p1.Player.inventory) === false;
	(List.mem bow p1.Player.equipped) === true;
	(List.mem sword p1.Player.inventory) === true;
	(List.mem sword p1.Player.equipped) === false;
	(* equip consumable into empty slot -> print done; remove new from inventroy; 
	add new to equipped *)
	let p1 = equip "dexterity potion" p1 in 
	(List.mem dexterity_potion p1.Player.equipped) === true;
	(List.mem dexterity_potion p1.Player.inventory) === false;
	(List.mem health_potion p1.Player.equipped) === false;
	(List.mem health_potion p1.Player.inventory) === true;
	(* equip consumable to full slot -> print done; remove first consumable from 
	equipped and add to inventory; add new consumable to end of equipped *)
	let p1 = equip "health potion" p1 in 
	(List.mem dexterity_potion p1.Player.equipped) === true;
	(List.mem dexterity_potion p1.Player.inventory) === false;
	(List.mem health_potion p1.Player.equipped) === true;
	(List.mem health_potion p1.Player.inventory) === true;	
	let p1 = equip "health potion" p1 in 
	(List.mem dexterity_potion p1.Player.equipped) === true;
	(List.mem dexterity_potion p1.Player.inventory) === false;
	(List.mem health_potion p1.Player.inventory) === true;
	(List.mem health_potion p1.Player.equipped) === true;
	let p1 = equip "health potion" p1 in 
	(List.mem dexterity_potion p1.Player.equipped) === false;
	(List.mem dexterity_potion p1.Player.inventory) === true;
	(List.mem health_potion p1.Player.equipped) === true;
	(List.mem health_potion p1.Player.inventory) === false;
	()

let test_remove = 
	(* remove item not equipped -> print cannot; loop back *)
	let p1 = remove "oboe" p1 in 
	List.length p1.Player.equipped === 1;
	(* remove item that is equipped -> print done; remove from equipped; 
	add to inventory *)
	let p1 = remove "sword" p1 in 
	(List.mem sword p1.Player.equipped) === false;
	(List.mem sword p1.Player.inventory) === true;
	()

(* let () = 
	let x = enter_shop test_shop p1 in 
	if (fst x).id = "Test Shop" then ()
	else () 
 *)


