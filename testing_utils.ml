let testdir = "InitGames/game1/"
let bow = Item.from_file testdir "Bow.json"
let sword = Item.from_file testdir "Sword.json"
let nail = Item.from_file testdir "Rusty Nail.json"
let health_potion = Item.from_file testdir "Health Potion.json"

let f0 = Fighter.from_file testdir "fighter0.json"
let f1 = Fighter.from_file testdir "fighter1.json"
let f2 = Fighter.from_file testdir "fighter2.json"
let f3 = Fighter.from_file testdir "fighter3.json"