type player
type item

type t = {
	supply: item list
}

val buy: t -> player -> player