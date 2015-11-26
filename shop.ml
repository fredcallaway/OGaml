
type t = {
	supply: Item.t list
}

val enter_shop: t -> Player.t -> Player.t



val buy: Item.t -> t -> Player.t -> Player.t

val sell: Item.t -> t -> Player.t -> Player.t

val equip: Item.t -> Player.t -> Player.t

val remove: Item.t -> Player.t -> Player.t
