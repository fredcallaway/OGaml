type player
type item

type t = {
	supply: item list
}

val buy: string -> t -> player -> player

val sell: string -> t -> player -> player

val equip: string -> player -> player

val remove: string -> player -> player
