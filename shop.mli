type player
type item

type t = {
	supply: item list
}

val buy: item -> t -> player -> player

val sell: item -> t -> player -> player

val equip: item -> player -> player

val remove: item -> player -> player

val shop_repl: player -> player